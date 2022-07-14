module Page.Agents exposing (match, page, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import Ident.View exposing (defaultIdentifier, displayIdentifier)
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Agent as A exposing (..)
import REA.Entity as EN exposing (Entity, toPluralString)
import Result exposing (andThen)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.Navbar as Navbar
import View.Radio as Radio


type alias Model =
    { route : Route }


type Msg
    = Removed Entity
    | Add


type alias Flags =
    { route : Route }


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
        Route.Agents ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed a ->
            ( model
            , Shared.dispatch s <| Event.Removed a
            )

        Add ->
            ( model, redirect s.navkey Route.AddAgent |> Effect.fromCmd )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Agents"
    , attributes = []
    , element = viewContent model Smallcard
    , route = Route.Agents
    }


viewContent : Model -> ViewType -> Shared.Model -> Element Msg
viewContent model vt s =
    case vt of
        Smallcard ->
            flatContent s
                "Agents"
                [ button.primary Add "Add..."
                ]
                [ wrappedRow
                    [ spacing 10 ]
                    (s.state.entities
                        |> Set.toList
                        |> List.map
                            (\e ->
                                viewSmallCard (Removed e)
                                    Nothing
                                    (defaultIdentifier s.state e Smallcard
                                        |> Maybe.map (displayIdentifier s.state)
                                        |> Maybe.withDefault (text <| EN.toString e)
                                    )
                                    ("Type: " ++ EN.toType e)
                            )
                        |> withDefaultContent (p "There are no Agents yet. Create your first one!")
                    )
                ]

        New ->
            text "New"
