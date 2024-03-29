module CommitmentType.ViewPage exposing (Flags, Model, Msg(..), match, page)

import CommitmentType.CommitmentType exposing (CommitmentType)
import CommitmentType.View
import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Expression.View
import Group.View exposing (displayGroupTable)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (getIdentifiers)
import Ident.View exposing (displayIdentifierDict)
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect)
import Scope.View
import Shared
import Spa.Page
import Type exposing (Type)
import Typed.Type as TType
import Util exposing (third)
import Value.Valuable exposing (getValues)
import Value.View exposing (displayValueDict)
import View exposing (..)
import Configuration.Zone.View exposing (displayZone)
import Configuration.Zone exposing (Zone(..))


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
        , view = view
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Entity Route.CommitmentType (Route.View p) ->
            Uuid.fromString p.uuid |> Maybe.map (Flags route)

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
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.CommitmentType <| Route.List {type_ = (Maybe.map Uuid.toString model.type_)} )

        Edit ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.CommitmentType <| Route.Edit {uuid = Uuid.toString model.uuid, type_ = Nothing} )


view : Model -> View Msg
view model =
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
        [ button.primary (Ok Edit) "Edit" ]
        [ h2 "Identifiers:"
        , text <| displayZone s.state SmallcardZone mainHType model.uuid
        , getIdentifiers s.state model.what model.uuid model.type_ False
            |> displayIdentifierDict "(none)"
        , h2 "Type:"
        , Dict.get (Uuid.toString model.uuid) s.state.types
            |> Maybe.andThen (\( _, _, mpuuid ) -> Maybe.map (\puuid -> displayZone s.state SmallcardZone mainHType puuid) mpuuid)
            |> Maybe.withDefault ""
            |> text
        , h2 "Values:"
        , getValues s.state.types s.state.valueTypes s.state.values model.what model.uuid model.type_ False
            |> displayValueDict s { context = ( Type.HType HType.CommitmentType, model.uuid ) } "(none)" s.state.values
        , h2 "Groups:"
        , model.groups
            |> List.map (\guuid -> displayZone s.state SmallcardZone (Type.TType TType.Group) guuid)
            |> displayGroupTable "(none)"
        , h2 "Default quantity"
        , model.ct |> Maybe.map .qty |> Maybe.map (Expression.View.inputExpression s.state { context = ( Type.HType HType.CommitmentType, model.uuid ) }) |> Maybe.withDefault (text "(none)")
        , h2 "Restrictions:"

        -- TODO what about resource conversions?
        , CommitmentType.View.svg
            (Maybe.map (.providers >> Scope.View.toDisplay s.state) model.ct |> Maybe.withDefault "(none)")
            (Maybe.map (.flowscope >> Scope.View.toDisplay s.state) model.ct |> Maybe.withDefault "(none)")
            (Maybe.map (.receivers >> Scope.View.toDisplay s.state) model.ct |> Maybe.withDefault "(none)")
        ]
