module Process.ViewPage exposing (..)

import Process.Process as Process exposing (Process)
import Configuration as Config
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable exposing (Groupable)
import Group.View exposing (displayGroupTable)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Ident.Identifiable as Identifiable exposing (withIdentifiers)
import Ident.View exposing (displayIdentifierDict)
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect, redirectParent)
import Scope.Scope as Scope exposing (Scope(..))
import Shared
import Spa.Page
import Type exposing (Type)
import Typed.Type as TType
import Typed.Typed as T
import View exposing (..)
import Zone.View exposing (display, hWithDisplay, tWithDisplay)
import Zone.Zone exposing (Zone(..))


type alias Flags =
    { route : Route
    , uuid : Maybe Uuid
    }


type alias Model =
    { route : Route
    , process : Maybe Process
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
        Route.ProcessView uuid ->
            Just { route = route, uuid = Uuid.fromString uuid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        mprocess =
            f.uuid |> Maybe.andThen (T.find s.state.processes)
    in
    ( { route = f.route
      , process = mprocess
      , groups =
            mprocess
                |> Maybe.map
                    (\process ->
                        s.state.grouped
                            |> Dict.filter (\_ v -> process.uuid == Groupable.uuid v.groupable)
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
            model.process
                |> Maybe.map
                    (\at ->
                        ( model, Effect.fromCmd <| redirect s.navkey (Route.ProcessEdit (Uuid.toString at.uuid)) )
                    )
                |> Maybe.withDefault ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding a Process Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    model.process
        |> Maybe.map
            (\t ->
                floatingContainer s
                    "Process"
                    [ button.primary Edit "Edit" ]
                    [ h2 "Parent type:"
                    , t.type_
                        |> H.find s.state.processTypes
                        |> Maybe.map (withIdentifiers s.state.identifiers)
                        |> Maybe.map (hWithDisplay s.state.processes s.state.processTypes s.state.configs SmallcardTitle)
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
                        |> List.map (Dict.get "SmallcardTitle" >> Maybe.withDefault "(none)")
                        |> displayGroupTable "(none)"
                    ]
            )
        |> Maybe.withDefault
            (floatingContainer s
                "Process"
                []
                [ h1 "Not found", text "The current URL does not correspond to anything" ]
            )
