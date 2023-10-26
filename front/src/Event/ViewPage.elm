module Event.ViewPage exposing (Flags, Model, Msg(..), match, page)

import Configuration.Zone exposing (Zone(..))
import Configuration.Zone.View exposing (displayZone)
import DateTime
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Expression exposing (Expression)
import Expression.Eval exposing (exeval)
import Expression.Rational as Rational
import Flow exposing (Flow)
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
import Time exposing (Posix, millisToPosix)
import Type exposing (Type)
import Typed.Type as TType
import Util exposing (orShowError, third)
import Value.Valuable exposing (getValues)
import Value.View exposing (displayValueDict)
import View exposing (..)


mainTType : Type
mainTType =
    Type.TType TType.Event


mainHType : Type
mainHType =
    Type.HType HType.EventType


type alias Flags =
    { route : Route
    , tuuid : Maybe String
    , uuid : Uuid
    }


type alias Model =
    { route : Route
    , what : Type
    , uuid : Uuid
    , provider : Maybe Uuid
    , receiver : Maybe Uuid
    , qty : Maybe Expression
    , flow : Maybe Flow
    , type_ : Maybe Uuid
    , groups : List Uuid
    , when : Posix
    , reconciliations : Dict String Reconciliation
    }


type Msg
    = Edit
    | Close
    | ViewProcess Uuid
    | Unreconciled Reconciliation


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
        Route.Entity Route.Event (Route.View p) ->
            Uuid.fromString p.uuid |> Maybe.map (Flags route p.type_)

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        event =
            Dict.get (Uuid.toString f.uuid) s.state.events
    in
    ( { route = f.route
      , what = mainTType
      , uuid = f.uuid
      , type_ = Maybe.andThen third (Dict.get (Uuid.toString f.uuid) s.state.types)
      , provider = Maybe.map .provider event
      , receiver = Maybe.map .receiver event
      , qty = Maybe.map .qty event
      , flow = Maybe.map .flow event
      , groups =
            s.state.grouped
                |> Dict.filter (\_ link -> link.groupable == f.uuid)
                |> Dict.values
                |> List.map (\link -> link.group)
      , when = Maybe.map .when event |> Maybe.withDefault (millisToPosix 0)
      , reconciliations =
            s.state.reconciliations
                |> Dict.filter (\_ r -> r.event == f.uuid)
      }
    , Effect.batch [ closeMenu f s.menu ]
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Close ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Event <| Route.List { type_ = Maybe.map Uuid.toString model.type_ } )

        Edit ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Event <| Route.Edit { uuid = Uuid.toString model.uuid, type_ = Maybe.map Uuid.toString model.type_ } )

        ViewProcess uuid ->
            ( model, Route.redirect s.navkey (Route.Entity Route.Process (Route.View { uuid = Uuid.toString uuid, type_ = Nothing })) |> Effect.fromCmd )

        Unreconciled r ->
            ( model
            , Shared.dispatch s (Payload.Unreconciled r)
            )


view : Model -> View Msg
view model =
    { title = "Event"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        rqty =
            model.qty
                |> Result.fromMaybe "(no expression)"
                |> Result.andThen (exeval s.state { context = ( Type.TType TType.Event, model.uuid ) } s.state.values)

        allocated =
            model.reconciliations |> Dict.foldl (\_ v q -> Rational.add v.qty q) Rational.zero

        unallocated =
            rqty |> Result.map (\qty -> Rational.add qty (Rational.neg allocated))
    in
    floatingContainer s
        (Just Close)
        "Event"
        [ button.primary (Ok Edit) "Edit" ]
        [ Dict.get (Uuid.toString model.uuid) s.state.types
            |> Maybe.andThen (\( _, _, mpuuid ) -> Maybe.map (\puuid -> displayZone s.state SmallcardZone mainHType puuid) mpuuid)
            |> Maybe.withDefault ""
            |> h1
        , h2 ("Date: " ++ DateTime.toString s.zone model.when)
        , h2
            ("What: "
                ++ (rqty
                        |> Result.map Rational.toFloatString
                        |> orShowError
                   )
                ++ " "
                ++ (model.flow |> Maybe.map (\f -> displayZone s.state SmallcardZone (Flow.userTypeOf f) (Flow.uuidOf f)) |> Maybe.withDefault "(none)")
            )
        , h2 <| "Unallocated: " ++ (unallocated |> Result.map Rational.toFloatString |> orShowError)
        , h2 ("Provider: " ++ (model.provider |> Maybe.map (displayZone s.state SmallcardZone (Type.TType TType.Agent)) |> Maybe.withDefault "(none)"))
        , h2 ("Receiver: " ++ (model.receiver |> Maybe.map (displayZone s.state SmallcardZone (Type.TType TType.Agent)) |> Maybe.withDefault "(none)"))
        , text <| displayZone s.state SmallcardZone mainTType model.uuid
        , getIdentifiers s.state model.what model.uuid model.type_ False
            |> displayIdentifierDict ""
        , h2 "Values:"
        , getValues s.state.types s.state.valueTypes s.state.values model.what model.uuid model.type_ False
            |> displayValueDict s { context = ( Type.TType TType.Event, model.uuid ) } "(none)" s.state.values
        , h2 "Groups:"
        , model.groups
            |> List.map (\guuid -> displayZone s.state SmallcardZone (Type.TType TType.Group) guuid)
            |> displayGroupTable "(none)"
        , h2 "Is part of the following processes :"
        , row [ spacing 5 ]
            (model.reconciliations
                |> Dict.values
                |> List.map
                    (\r ->
                        Process.Reconcile.View.viewAsProcess s.state ViewProcess Unreconciled r
                    )
            )
        ]
