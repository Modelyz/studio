module Group.ViewPage exposing (Flags, Model, Msg(..), match, page)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable
import Hierarchy.Hierarchic as H
import Ident.Identifiable exposing (withIdentifiers)
import Ident.View exposing (displayIdentifierDict)
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Typed.Typed as T
import View exposing (..)
import Zone.View exposing (hWithDisplay)
import Zone.Zone exposing (Zone(..))


type alias Flags =
    { route : Route
    , uuid : Maybe Uuid
    }


type alias Model =
    { route : Route
    , group : Maybe Group
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
        Route.GroupView uuid ->
            Just { route = route, uuid = Uuid.fromString uuid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        mgroup =
            f.uuid |> Maybe.andThen (T.find s.state.groups)
    in
    ( { route = f.route
      , group = mgroup
      , groups =
            mgroup
                |> Maybe.map
                    (\group ->
                        s.state.grouped
                            |> Dict.filter (\_ v -> group.uuid == Groupable.uuid v.groupable)
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
            model.group
                |> Maybe.map
                    (\at ->
                        ( model, Effect.fromCmd <| redirect s.navkey (Route.GroupEdit (Uuid.toString at.uuid)) )
                    )
                |> Maybe.withDefault ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding a Group Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    model.group
        |> Maybe.map
            (\t ->
                floatingContainer s
                    "Group"
                    [ button.primary Edit "Edit" ]
                    [ h2 "Parent type:"
                    , t.type_
                        |> H.find s.state.groupTypes
                        |> Maybe.map (withIdentifiers s.state.agents s.state.agentTypes s.state.identifierTypes s.state.identifiers)
                        |> Maybe.map (hWithDisplay s.state.groups s.state.groupTypes s.state.configs SmallcardTitle)
                        |> Maybe.map .display
                        |> Maybe.andThen (Dict.get "SmallcardTitle")
                        |> Maybe.withDefault "(none)"
                        |> text
                    , h2 "Identifiers:"
                    , t
                        |> withIdentifiers s.state.agents s.state.agentTypes s.state.identifierTypes s.state.identifiers
                        |> .identifiers
                        |> displayIdentifierDict "(none)"
                    ]
            )
        |> Maybe.withDefault
            (floatingContainer s
                "Group"
                []
                [ h1 "Not found", text "The current URL does not correspond to anything" ]
            )
