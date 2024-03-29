module Process.ViewPage exposing (Flags, Model, Msg(..), match, page)

import Configuration.Zone exposing (Zone(..))
import Configuration.Zone.View exposing (displayZone)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Group.View exposing (displayGroupTable)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (getIdentifiers)
import Ident.View exposing (displayIdentifierDict)
import Payload exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Reconcile exposing (Reconciliation)
import Process.Reconcile.View
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Type exposing (Type)
import Typed.Type as TType
import Util exposing (flip, third)
import Value.Valuable exposing (getValues)
import Value.View exposing (displayValueDict)
import View exposing (..)


mainTType : Type
mainTType =
    Type.TType TType.Process


mainHType : Type
mainHType =
    Type.HType HType.ProcessType


type alias Flags =
    { route : Route
    , tuuid : Maybe String
    , uuid : Uuid
    }


type alias Model =
    { route : Route
    , what : Type
    , uuid : Uuid
    , reconciliations : Dict String Reconciliation
    , type_ : Maybe Uuid
    , groups : List Uuid
    }


type Msg
    = Edit
    | ViewEvent Uuid
    | Unreconciled Reconciliation
    | Close
    | Add { type_ : Uuid, related : Maybe Uuid }


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
        Route.Entity Route.Process (Route.View p) ->
            Uuid.fromString p.uuid |> Maybe.map (Flags route p.type_)

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , what = mainTType
      , uuid = f.uuid
      , reconciliations =
            s.state.reconciliations
                |> Dict.filter (\_ r -> r.process == f.uuid)
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
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Process <| Route.List { type_ = Maybe.map Uuid.toString model.type_ } )

        Edit ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Process <| Route.Edit { uuid = Uuid.toString model.uuid, type_ = Maybe.map Uuid.toString model.type_ } )

        ViewEvent uuid ->
            ( model, Route.redirect s.navkey (Route.Entity Route.Event (Route.View { uuid = Uuid.toString uuid, type_ = Nothing })) |> Effect.fromCmd )

        Unreconciled r ->
            ( model
            , Shared.dispatch s (Payload.Unreconciled r)
            )

        Add p ->
            ( model, Route.redirect s.navkey (Route.Entity Route.Event (Route.Add { type_ = Just (Uuid.toString p.type_), related = Maybe.map Uuid.toString p.related, step = Nothing })) |> Effect.fromCmd )


view : Shared.Model -> Model -> View Msg
view _ model =
    { title = "Process"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    floatingContainer s
        (Just Close)
        "Process"
        [ button.primary (Ok Edit) "Edit" ]
        [ Dict.get (Uuid.toString model.uuid) s.state.types
            |> Maybe.andThen (\( _, _, mpuuid ) -> Maybe.map (\puuid -> displayZone s.state SmallcardZone mainHType puuid) mpuuid)
            |> Maybe.withDefault ""
            |> h1
        , text <| displayZone s.state SmallcardZone mainTType model.uuid
        , getIdentifiers s.state model.what model.uuid model.type_ False
            |> displayIdentifierDict "(none)"
        , h2 "Values:"
        , getValues s.state.types s.state.valueTypes s.state.values model.what model.uuid model.type_ False
            |> displayValueDict s { context = ( Type.TType TType.Process, model.uuid ) } "(none)" s.state.values
        , h2 "Groups:"
        , model.groups
            |> List.map (\guuid -> displayZone s.state SmallcardZone (Type.TType TType.Group) guuid)
            |> displayGroupTable "(none)"
        , h2 "Contains the following Events:"
        , row [ spacing 5 ]
            (model.reconciliations
                |> Dict.values
                |> List.map
                    (\r ->
                        Process.Reconcile.View.viewAsEvent s.state ViewEvent Unreconciled r
                    )
            )
        , row [ spacing 5 ] <|
            Dict.values <|
                Dict.map (\_ etuuid -> button.primary (Ok (Add { type_ = etuuid, related = Just model.uuid })) ("Add " ++ displayZone s.state SmallcardZone (Type.HType HType.EventType) etuuid))
                    (model.type_
                        |> Maybe.map Uuid.toString
                        |> Maybe.andThen (flip Dict.get s.state.processTypes)
                        |> Maybe.map .eventTypes
                        |> Maybe.withDefault Dict.empty
                    )
        ]
