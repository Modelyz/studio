module ProcessType.ViewPage exposing (Flags, Model, Msg(..), match, page)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable
import Group.View exposing (displayGroupTable)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (getIdentifiers)
import Ident.View exposing (displayIdentifierDict)
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process exposing (Process)
import ProcessType.ProcessType exposing (ProcessType)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import State
import Type exposing (Type)
import Value.Input exposing (inputValues)
import Value.Valuable exposing (getValues)
import Value.Value as Value exposing (Value)
import Value.ValueType exposing (initValues)
import Value.View exposing (displayValueDict)
import View exposing (..)
import Zone.View exposing (display)
import Zone.Zone exposing (Zone(..))


mainHType : Type
mainHType =
    Type.HType HType.ProcessType


type alias Flags =
    { route : Route
    , uuid : Uuid
    }


type alias Model =
    { route : Route
    , what : Type
    , uuid : Uuid
    , groups : List ( Type, Uuid )
    }


type Msg
    = Edit
    | Close


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
        Route.Entity Route.ProcessType (Route.View uuid) ->
            Uuid.fromString uuid |> Maybe.map (Flags route)

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , what = mainHType
      , uuid = f.uuid
      , groups =
            s.state.grouped
                |> Dict.filter (\_ link -> link.groupable == f.uuid)
                |> Dict.values
                |> List.map (\link -> ( link.what, link.groupable ))
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Close ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.ProcessType <| Route.List Nothing )

        Edit ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.ProcessType <| Route.Edit (Uuid.toString model.uuid) )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Process Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    floatingContainer s
        (Just Close)
        "Process Type"
        [ button.primary Edit "Edit" ]
        [ h2 "Type:"
        , Dict.get (Uuid.toString model.uuid) s.state.types
            |> Maybe.andThen (\( _, _, mpuuid ) -> Maybe.map (\puuid -> display s.state.types s.state.configs SmallcardTitle s.state.identifiers mainHType puuid) mpuuid)
            |> Maybe.withDefault ""
            |> text
        , h2 "Identifiers:"
        , getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers model.what model.uuid
            |> displayIdentifierDict "(none)"
        , h2 "Values:"
        , getValues s.state.types s.state.valueTypes s.state.values model.what model.uuid
            |> displayValueDict "(none)" s.state.values
        , h2 "Groups:"
        , model.groups
            |> List.map (\( gt, guuid ) -> display s.state.types s.state.configs SmallcardTitle s.state.identifiers gt guuid)
            |> displayGroupTable "(none)"
        ]
