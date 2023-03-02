module Zone.AddPage exposing (Flags, Model, Msg(..), match, page)

import Configuration exposing (Configuration(..))
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Message
import Route exposing (Route, redirect)
import Scope as Scope exposing (Scope(..))
import Scope.View exposing (selectScope)
import Shared
import Spa.Page
import Util exposing (checkEmptyList)
import View exposing (..)
import View.MultiSelect exposing (multiSelect)
import View.Style exposing (..)
import Zone.Fragment as Fragment exposing (Fragment(..))
import Zone.View exposing (allFragmentsByScope)
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
    , errors : Dict Int String
    , warning : String
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init s
        , update = update s
        , view = view
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Entity Route.Configuration (Route.Add _) ->
            Just { route = route, zid = "" }

        Route.Entity Route.Configuration (Route.Edit zid _) ->
            Just { route = route, zid = zid }

        _ ->
            Nothing


view : Model -> View Msg
view model =
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
            , scope = Scope.anything
            , fragments = []
            , errors = Dict.empty
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
        , selectScope s.state InputScope model.scope Scope.anything "What should it apply to?"
        , multiSelect
            model
            { inputMsg = InputFragments
            , selection = .fragments
            , title = "Format: "
            , description = "Click on the items below to construct the display of your identifier"
            , toString = Fragment.toString
            , toDesc = Fragment.toDesc
            , height = 100
            , input = inputFragment
            }
          <|
            allFragmentsByScope s.state model.scope
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


inputFragment : List Fragment -> Int -> Fragment -> Element Msg
inputFragment fragments index fragment =
    case fragment of
        Fixed value ->
            Input.text [ width (px 75), height shrink, padding 5, spacing 5 ]
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
                , placeholder = Just <| Input.placeholder [] <| text <| Fragment.toString fragment
                , label = Input.labelHidden "Fixed"
                }

        Parent ->
            none

        IdentifierName _ ->
            none

        GroupIdentifierName _ ->
            none

        Quantity ->
            none

        Flow ->
            none

        Provider ->
            none

        Receiver ->
            none
