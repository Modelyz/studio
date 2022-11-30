module Zone.AddPage exposing (Flags, Model, Msg(..), match, page)

import Configuration exposing (Configuration(..))
import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Message
import Route exposing (Route, redirect)
import Scope.Scope as Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Scope.View exposing (selectScope)
import Shared
import Spa.Page
import Type
import Typed.Type as TType
import Util exposing (checkEmptyList)
import View exposing (..)
import View.Smallcard exposing (clickableCard)
import View.Style exposing (..)
import Zone.Fragment as Fragment exposing (Fragment(..))
import Zone.Zone as Zone exposing (Zone(..))


type Msg
    = InputZone Zone
    | InputScope Scope
    | InputFragments (List Fragment)
    | Cancel
    | Added


type alias Flags =
    { route : Route, zid : String }


type alias Model =
    { route : Route
    , isNew : Bool
    , zone : Zone
    , scope : Scope
    , fragments : List Fragment
    , warning : String
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init s
        , update = update s
        , view = view s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Entity Route.Configuration Route.Add ->
            Just { route = route, zid = "" }

        Route.Entity Route.Configuration (Route.Edit zid) ->
            Just { route = route, zid = zid }

        _ ->
            Nothing


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding a Zone display format"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        isNew =
            f.zid == ""

        adding =
            { route = f.route
            , isNew = isNew
            , zone = SmallcardTitle
            , scope = Scope.empty
            , fragments = []
            , warning = ""
            }
    in
    (if isNew then
        adding

     else
        Dict.get f.zid s.state.configs
            |> Maybe.map
                (\(ZoneConfig zone fragments scope) ->
                    { adding
                        | zone = zone
                        , scope = scope
                        , fragments = fragments
                        , warning = ""
                    }
                )
            |> Maybe.withDefault adding
    )
        |> Effect.with (closeMenu f s.menu)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputZone zone ->
            { model
                | zone = zone
                , fragments =
                    if zone == model.zone then
                        model.fragments

                    else
                        []
            }
                |> Effect.withNone

        InputScope scope ->
            { model
                | scope = scope
                , fragments =
                    if scope == model.scope then
                        model.fragments

                    else
                        []
            }
                |> Effect.withNone

        InputFragments fragments ->
            { model | fragments = fragments } |> Effect.withNone

        Cancel ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Configuration (Route.List Nothing) )

        Added ->
            case validate model of
                Ok c ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatch s <| Message.Configured c
                        , Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Configuration (Route.List Nothing)
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )


validate : Model -> Result String Configuration
validate m =
    Result.map3 ZoneConfig
        (Ok m.zone)
        (checkEmptyList m.fragments "You must choose at least an identifier")
        (if m.scope == Scope.empty then
            Err "You must choose a scope"

         else
            Ok m.scope
        )


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        buttons : List (Element Msg)
        buttons =
            [ wrappedRow [ width fill, spacing 20 ]
                [ button.secondary Cancel "Cancel"
                , button.primary Added "Validate and finish"
                , if model.warning /= "" then
                    paragraph [ Font.color color.text.warning ] [ text model.warning ]

                  else
                    none
                ]
            ]
    in
    floatingContainer s
        (Just <| Cancel)
        "Adding a Display Zone Configuration"
        buttons
        [ inputZone model
        , selectScope s InputScope model.scope Scope.empty "What should it apply to?"
        , inputFragments s model
        ]


inputZone : Model -> Element Msg
inputZone model =
    wrappedRow [ spacing 10 ] <|
        List.map
            (\zone ->
                row
                    [ if zone == model.zone then
                        Background.color color.item.selected

                      else
                        Background.color color.item.background
                    , padding 10
                    , pointer
                    , onClick <| InputZone zone
                    ]
                    [ Zone.toDesc zone |> text ]
            )
            Zone.all


inputFragments : Shared.Model -> Model -> Element Msg
inputFragments s model =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0 ] <| h2 "Format: ")
                :: List.append
                    (if List.isEmpty model.fragments then
                        [ el [ padding 10, Font.color color.text.disabled ] (text "Empty") ]

                     else
                        []
                    )
                    (model.fragments
                        |> List.indexedMap
                            (\i fragment ->
                                row [ Background.color color.item.background ]
                                    [ button.primary
                                        (InputFragments
                                            (model.fragments
                                                |> List.indexedMap Tuple.pair
                                                |> List.filter (\( j, _ ) -> j /= i)
                                                |> List.map Tuple.second
                                            )
                                        )
                                        "×"
                                    , el [ paddingXY 10 2 ] (text <| Fragment.toString fragment)
                                    , inputFragment model.fragments i fragment
                                    ]
                            )
                    )
        , h2 "Click on the items below to construct the format of the zone"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            List.map
                (\f ->
                    clickableCard
                        (InputFragments <| model.fragments ++ [ f ])
                        (text <| Fragment.toDesc f)
                        (text <| Fragment.toString f)
                )
                (Fixed ""
                    :: ((s.state.identifierTypes
                            |> Dict.values
                            |> List.filter
                                (\it -> containsScope s.state.types model.scope it.scope)
                        )
                            |> List.map (.name >> IdentifierName)
                       )
                    ++ ((s.state.identifierTypes
                            |> Dict.values
                            |> List.filter
                                (\it -> containsScope s.state.types it.scope (HasType (Type.TType TType.Group)))
                        )
                            |> List.map (.name >> GroupIdentifierName)
                       )
                )
        ]


inputFragment : List Fragment -> Int -> Fragment -> Element Msg
inputFragment fragments index fragment =
    case fragment of
        Fixed value ->
            Input.text [ width (px 75) ]
                { onChange =
                    \v ->
                        InputFragments
                            (fragments
                                |> List.indexedMap
                                    (\i f ->
                                        if i == index then
                                            Fixed v

                                        else
                                            f
                                    )
                            )
                , text = value
                , placeholder =
                    Just <| Input.placeholder [] <| text <| Fragment.toString fragment
                , label = Input.labelHidden <| "Fixed"
                }

        IdentifierName _ ->
            none

        GroupIdentifierName _ ->
            none
