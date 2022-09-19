module EventType.ViewPage exposing (..)

import EventType.EventType as EventType exposing (EventType)
import Configuration as Config
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable exposing (Groupable)
import Group.View exposing (displayGroupTable)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Hierarchy.Type as HType
import Ident.Identifiable as Identifiable exposing (withIdentifiers)
import Ident.View exposing (displayIdentifierDict)
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect, redirectParent)
import Scope.Scope as Scope exposing (Scope(..))
import Shared
import Spa.Page
import Type exposing (Type)
import Typed.Type as TType
import View exposing (..)
import Zone.View exposing (display, hWithDisplay, tWithDisplay)
import Zone.Zone exposing (Zone(..))


type alias Flags =
    { route : Route
    , uuid : Maybe Uuid
    }


type alias Model =
    { route : Route
    , eventType : Maybe EventType
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
        Route.EventTypeView uuid ->
            Just { route = route, uuid = Uuid.fromString uuid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        meventType =
            f.uuid |> Maybe.andThen (H.find s.state.eventTypes)
    in
    ( { route = f.route
      , eventType = f.uuid |> Maybe.andThen (H.find s.state.eventTypes)
      , groups =
            meventType
                |> Maybe.map
                    (\event ->
                        s.state.grouped
                            |> Dict.filter (\_ v -> event.uuid == Groupable.uuid v.groupable)
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
            model.eventType
                |> Maybe.map
                    (\h ->
                        ( model, Effect.fromCmd <| redirect s.navkey (Route.EventTypeEdit (Uuid.toString h.uuid)) )
                    )
                |> Maybe.withDefault ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Event Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    model.eventType
        |> Maybe.map
            (\h ->
                floatingContainer s
                    "EventType"
                    [ button.primary Edit "Edit" ]
                    [ h2 "Parent type:"
                    , h.parent
                        |> Maybe.andThen (H.find s.state.eventTypes)
                        |> Maybe.map (withIdentifiers s.state.identifiers)
                        |> Maybe.map (hWithDisplay s.state.events s.state.eventTypes s.state.configs SmallcardTitle)
                        |> Maybe.map .display
                        |> Maybe.andThen (Dict.get "SmallcardTitle")
                        |> Maybe.withDefault "(none)"
                        |> text
                    , h2 "Identifiers:"
                    , h
                        |> withIdentifiers s.state.identifiers
                        |> .identifiers
                        |> displayIdentifierDict "(none)"
                    , h2 "Groups:"
                    , model.groups
                        |> Dict.values
                        |> List.map (withIdentifiers s.state.identifiers)
                        |> List.map (tWithDisplay s.state.groups s.state.groupTypes s.state.configs SmallcardTitle)
                        |> List.map .display
                        |> List.map (Dict.get "SmallcardTitle" >> Maybe.withDefault "(none)")
                        |> displayGroupTable "(none)"
                    ]
            )
        |> Maybe.withDefault
            (floatingContainer s
                "EventType"
                []
                [ h1 "Not found", text "The current URL does not correspond to anything" ]
            )
