module Zone.AddPage exposing (..)

import Configuration exposing (Configuration(..), only)
import DictSet as Set exposing (DictSet)
import Effect as Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Entity.Entity as Entity
import Entity.Type as Type exposing (Type(..))
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Ident.Scope as Scope exposing (Scope(..))
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect, redirectParent)
import Shared
import Spa.Page
import View exposing (..)
import View.Style exposing (..)
import Zone.Fragment as Fragment exposing (Fragment(..))
import Zone.Zone as Zone exposing (Zone(..))


type Msg
    = InputZone Zone
    | InputScopes (DictSet String Scope)
    | InputFragments (List Fragment)
    | Warning String
    | Cancel
    | Added


type alias Flags =
    { route : Route }


type alias Model =
    { route : Route
    , zone : Zone
    , scopes : DictSet String Scope
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
        Route.ZoneConfigAdd ->
            Just { route = route }

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
    { route = f.route
    , zone = SmallcardItemTitle
    , scopes = Set.empty Scope.compare
    , fragments = []
    , warning = ""
    }
        |> Effect.with (closeMenu f s.menu)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputZone zone ->
            { model | zone = zone } |> Effect.withNone

        InputScopes scopes ->
            { model | scopes = scopes } |> Effect.withNone

        InputFragments fragments ->
            { model | fragments = fragments } |> Effect.withNone

        Warning string ->
            { model | warning = string } |> Effect.withNone

        Cancel ->
            ( model, redirect s.navkey Route.ConfigurationList |> Effect.fromCmd )

        Added ->
            case validate model of
                Ok c ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatch s <| Message.Configured c
                        , redirect s.navkey Route.ConfigurationList |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )


validate : Model -> Result String Configuration
validate m =
    Result.map3 ZoneConfig
        (Ok m.zone)
        (checkEmptyList m.fragments "You must choose at least an identifier")
        (checkEmptyDict m.scopes "You must choose en entity type")


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
        "Adding a Display Zone Configuration"
        buttons
        [ inputZone model
        , inputScopes s model
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


viewItem : Model -> Scope -> Element Msg
viewItem model i =
    -- TODO duplicated from Ident/AddPage
    row [ Background.color color.item.background ]
        [ el [ paddingXY 10 2 ] (text <| Scope.toDesc i)
        , button.primary (InputScopes <| Set.remove i model.scopes) "×"
        ]


inputScopes : Shared.Model -> Model -> Element Msg
inputScopes s model =
    -- TODO duplicated from Ident/AddPage → refactor
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Apply to: ")
                :: List.append
                    (if Set.isEmpty model.scopes then
                        [ el [ padding 10, Font.color color.text.main ] (text <| "Nothing") ]

                     else
                        []
                    )
                    (model.scopes |> Set.toList |> List.map (viewItem model))
        , h2 <| "Select the types of the entities you want to configure the zone for"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (Type.all
                |> List.map
                    (\t ->
                        clickableCard (InputScopes <| Set.insert (AllEntities t) model.scopes) ("All " ++ Type.toPluralString t) Nothing
                    )
            )
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (s.state.entities
                |> Set.toList
                |> List.map
                    (\e ->
                        clickableCard
                            (InputScopes <| Set.insert (AllEntitiesOfType (Entity.toUuid e)) model.scopes)
                            (Entity.toUuidString e)
                            (Entity.toTypeUuid e
                                |> Maybe.map (\u -> "Type: " ++ Uuid.toString u)
                            )
                    )
            )
        ]


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
                    column
                        [ Background.color color.item.background
                        , mouseOver itemHoverstyle
                        , onClick (InputFragments <| model.fragments ++ [ f ])
                        , pointer
                        , padding 10
                        , spacing 10
                        , height (px 75)
                        ]
                        [ el [ width <| minimum 100 fill ] (text <| Fragment.toString f)
                        , paragraph [ Font.size size.text.main ] [ text <| Fragment.toDesc f ]
                        ]
                )
                (Fixed ""
                    :: (s.state.identifierTypes
                            |> Set.filter
                                (\it ->
                                    List.all
                                        (\sc ->
                                            List.all
                                                (\sc2 ->
                                                    Scope.isParentOf sc2 s.state.entities sc
                                                )
                                                (Set.toList model.scopes)
                                        )
                                        (Set.toList it.applyTo)
                                )
                            |> Set.toList
                            |> List.map (.name >> IdentifierName)
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

        IdentifierName name ->
            none
