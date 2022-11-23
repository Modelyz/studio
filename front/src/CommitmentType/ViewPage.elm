module CommitmentType.ViewPage exposing (Flags, Model, Msg(..), match, page)

import CommitmentType.CommitmentType exposing (CommitmentType)
import Dict
import Effect exposing (Effect)
import Element as E exposing (..)
import Group.View exposing (displayGroupTable)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (getIdentifiers)
import Ident.View exposing (displayIdentifierDict)
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect)
import Scope.View
import Shared
import Spa.Page
import Svg as S
import Svg.Attributes as A
import Type exposing (Type)
import Typed.Type as TType
import Util exposing (third)
import Value.Valuable exposing (getValues)
import Value.View exposing (displayValueDict)
import View exposing (..)
import View.Style as Style
import Zone.View exposing (display)
import Zone.Zone exposing (Zone(..))


mainHType : Type
mainHType =
    Type.HType HType.CommitmentType


type alias Flags =
    { route : Route
    , uuid : Uuid
    }


type alias Model =
    { route : Route
    , what : Type
    , uuid : Uuid
    , ct : Maybe CommitmentType
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
        Route.Entity Route.CommitmentType (Route.View uuid) ->
            Uuid.fromString uuid |> Maybe.map (Flags route)

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , what = mainHType
      , uuid = f.uuid
      , ct = Dict.get (Uuid.toString f.uuid) s.state.commitmentTypes
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
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.CommitmentType <| Route.List Nothing )

        Edit ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.CommitmentType <| Route.Edit (Uuid.toString model.uuid) )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Commitment Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    floatingContainer s
        (Just Close)
        "Commitment Type"
        [ button.primary Edit "Edit" ]
        [ h2 "Identifiers:"
        , getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers model.what model.uuid model.type_ False
            |> displayIdentifierDict "(none)"
        , h2 "Type:"
        , Dict.get (Uuid.toString model.uuid) s.state.types
            |> Maybe.andThen (\( _, _, mpuuid ) -> Maybe.map (\puuid -> display s.state.types s.state.configs SmallcardTitle s.state.identifiers mainHType puuid) mpuuid)
            |> Maybe.withDefault ""
            |> text
        , h2 "Values:"
        , getValues s.state.types s.state.valueTypes s.state.values model.what model.uuid model.type_ False
            |> displayValueDict "(none)" s.state.values
        , h2 "Groups:"
        , model.groups
            |> List.map (\guuid -> display s.state.types s.state.configs SmallcardTitle s.state.identifiers (Type.TType TType.Group) guuid)
            |> displayGroupTable "(none)"
        , h2 "Restrictions:"

        -- TODO what about resource conversions?
        , commitment
            (Maybe.map (.providers >> Scope.View.toDisplay s) model.ct |> Maybe.withDefault "(none)")
            (Maybe.map (.flow >> Scope.View.toDisplay s) model.ct |> Maybe.withDefault "(none)")
            (Maybe.map (.receivers >> Scope.View.toDisplay s) model.ct |> Maybe.withDefault "(none)")
        ]
