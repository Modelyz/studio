module AgentType.ViewPage exposing (Flags, Model, Msg(..), match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Group.View exposing (displayGroupTable)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (getIdentifiers)
import Ident.View exposing (displayIdentifierDict)
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Type exposing (Type)
import Typed.Type as TType
import Util exposing (third)
import Value.Valuable exposing (getValues)
import Value.View exposing (displayValueDict)
import View exposing (..)
import Zone.View exposing (displayZone)
import Zone.Zone exposing (Zone(..))


mainHType : Type
mainHType =
    Type.HType HType.AgentType


type alias Flags =
    { route : Route
    , uuid : Uuid
    }


type alias Model =
    { route : Route
    , what : Type
    , uuid : Uuid
    , type_ : Maybe Uuid
    , groups : List Uuid
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
        Route.Entity Route.AgentType (Route.View uuid _) ->
            Uuid.fromString uuid |> Maybe.map (Flags route)

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , what = mainHType
      , uuid = f.uuid
      , type_ = Maybe.andThen third (Dict.get (Uuid.toString f.uuid) s.state.types)
      , groups =
            s.state.grouped
                |> Dict.filter (\_ link -> link.groupable == f.uuid)
                |> Dict.values
                |> List.map (\link -> link.group)
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Close ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.AgentType <| Route.List (Maybe.map Uuid.toString model.type_) )

        Edit ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.AgentType <| Route.Edit (Uuid.toString model.uuid) Nothing )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Agent Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    floatingContainer s
        (Just Close)
        "Agent Type"
        [ button.primary Edit "Edit" ]
        [ h2 "Identifiers:"
        , text <| displayZone s.state s.state.types s.state.configs SmallcardTitle s.state.identifiers s.state.grouped s.state.groups mainHType model.uuid
        , getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers model.what model.uuid model.type_ False
            |> displayIdentifierDict "(none)"
        , h2 "Type:"
        , Dict.get (Uuid.toString model.uuid) s.state.types
            |> Maybe.andThen (\( _, _, mpuuid ) -> Maybe.map (\puuid -> displayZone s.state s.state.types s.state.configs SmallcardTitle s.state.identifiers s.state.grouped s.state.groups mainHType puuid) mpuuid)
            |> Maybe.withDefault ""
            |> text
        , h2 "Values:"
        , getValues s.state.types s.state.valueTypes s.state.values model.what model.uuid model.type_ False
            |> displayValueDict s { context = ( Type.HType HType.AgentType, model.uuid ) } "(none)" s.state.values
        , h2 "Groups:"
        , model.groups
            |> List.map (\guuid -> displayZone s.state s.state.types s.state.configs SmallcardTitle s.state.identifiers s.state.grouped s.state.groups (Type.TType TType.Group) guuid)
            |> displayGroupTable "(none)"
        ]
