module Agent.ViewPage exposing (Flags, Model, Msg(..), match, page)

import Agent.Agent exposing (Agent)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable
import Group.View exposing (displayGroupTable)
import Hierarchy.Hierarchic as H
import Ident.Identifiable exposing (withIdentifiers)
import Ident.View exposing (displayIdentifierDict)
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Typed.Typed as T
import View exposing (..)
import Zone.View exposing (hWithDisplay, tWithDisplay)
import Zone.Zone exposing (Zone(..))


type alias Flags =
    { route : Route
    , uuid : Maybe Uuid
    }


type alias Model =
    { route : Route
    , agent : Maybe Agent
    , groups : Dict String Group
    }


type Msg
    = Edit


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
        Route.AgentView uuid ->
            Just { route = route, uuid = Uuid.fromString uuid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        magent =
            f.uuid |> Maybe.andThen (T.find s.state.agents)
    in
    ( { route = f.route
      , agent = magent
      , groups =
            magent
                |> Maybe.map
                    (\agent ->
                        s.state.grouped
                            |> Dict.filter (\_ v -> agent.uuid == Groupable.uuid v.groupable)
                            |> Dict.foldl (\_ v d -> Dict.insert (Group.compare v.group) v.group d) Dict.empty
                    )
                |> Maybe.withDefault Dict.empty
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Edit ->
            model.agent
                |> Maybe.map
                    (\at ->
                        ( model, Effect.fromCmd <| redirect s.navkey (Route.AgentEdit (Uuid.toString at.uuid)) )
                    )
                |> Maybe.withDefault ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Agent Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    model.agent
        |> Maybe.map
            (\t ->
                floatingContainer s
                    "Agent"
                    [ button.primary Edit "Edit" ]
                    [ h2 "Parent type:"
                    , t.type_
                        |> H.find s.state.agentTypes
                        |> Maybe.map (withIdentifiers s.state.identifiers)
                        |> Maybe.map (hWithDisplay s.state.agents s.state.agentTypes s.state.configs SmallcardTitle)
                        |> Maybe.map .display
                        |> Maybe.andThen (Dict.get "SmallcardTitle")
                        |> Maybe.withDefault "(none)"
                        |> text
                    , h2 "Identifiers:"
                    , t
                        |> withIdentifiers s.state.identifiers
                        |> .identifiers
                        |> displayIdentifierDict "(none)"
                    , h2 "Groups:"
                    , model.groups
                        |> Dict.values
                        |> List.map (withIdentifiers s.state.identifiers)
                        |> List.map (tWithDisplay s.state.groups s.state.groupTypes s.state.configs SmallcardTitle)
                        |> List.map .display
                        |> List.map (Dict.get "SmallcardTitle" >> Maybe.withDefault "(missing zone config)")
                        |> displayGroupTable "(none)"
                    ]
            )
        |> Maybe.withDefault
            (floatingContainer s
                "Agent"
                []
                [ h1 "Not found", text "The current URL does not correspond to anything" ]
            )
