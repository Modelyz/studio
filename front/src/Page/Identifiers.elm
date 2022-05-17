module Page.Identifiers exposing (match, page, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.Entity as Entity exposing (Entity, toPluralString)
import REA.Identifier as I exposing (..)
import REA.Identifier.Portion as Portion exposing (Portion(..))
import Result exposing (andThen)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.Navbar as Navbar
import View.Radio as Radio


type alias Model =
    { route : Route }


type Msg
    = Removed String
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
        Route.Identifiers ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route }, closeMenu s )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed i ->
            ( model
            , Shared.dispatch s <| Event.IdentifierRemoved i
            )

        Add ->
            ( model, goTo s Route.AddIdentifier )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Identifiers"
    , attributes = []
    , element = viewContent model
    , route = Route.Identifiers
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    flatContent s
        "Identifiers"
        [ button.primary Add "Add..."
        ]
        [ wrappedRow
            [ spacing 10 ]
            (s.state.identifiers
                |> Set.toList
                |> List.sortBy .name
                |> List.map (\i -> viewSmallCard (Removed i.name) i.name ("for " ++ Entity.toPluralString i.entity))
                |> withDefaultContent (p "There are no Identifiers yet. Create your first one!")
            )
        ]
