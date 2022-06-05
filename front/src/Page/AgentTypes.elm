module Page.AgentTypes exposing (match, page, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.AgentType as AT exposing (..)
import REA.Entity as Entity exposing (Entity, toPluralString)
import REA.EntityType as ENT exposing (EntityType)
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
    = Removed EntityType
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
        Route.AgentTypes ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route }, closeMenu s )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed et ->
            ( model
            , Shared.dispatch s <| Event.TypeRemoved et
            )

        Add ->
            ( model, redirect s Route.AddAgentType )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Agent Types"
    , attributes = []
    , element = viewContent model
    , route = Route.AgentTypes
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        entityTypes =
            Set.filter (\et -> ENT.toString et == "AgentType") s.state.entityTypes
    in
    flatContent s
        "AgentTypes"
        [ button.primary Add "Add..."
        ]
        [ wrappedRow
            [ spacing 10 ]
            (entityTypes
                |> Set.toList
                |> List.map (\et -> viewSmallCard (Removed et) (ENT.toName et) <| Maybe.withDefault "(Root type)" <| Maybe.map (\t -> "Type: " ++ t) (ENT.toParent et))
                |> withDefaultContent (p "There are no AgentTypes yet. Create your first one!")
            )
        ]
