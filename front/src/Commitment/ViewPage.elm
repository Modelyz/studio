module Commitment.ViewPage exposing (Flags, Model, Msg(..), match, page)

import DateTime
import Dict
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
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Time exposing (Posix, millisToPosix)
import Type exposing (Type)
import Typed.Type as TType
import Util exposing (third)
import Value.Valuable exposing (getValues)
import Value.View exposing (displayValueDict)
import View exposing (..)
import Configuration.Zone.View exposing (displayZone)
import Configuration.Zone exposing (Zone(..))


mainTType : Type
mainTType =
    Type.TType TType.Commitment


mainHType : Type
mainHType =
    Type.HType HType.CommitmentType


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
        Route.Entity Route.Commitment (Route.View uuid tuuid) ->
            Uuid.fromString uuid |> Maybe.map (Flags route tuuid)

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        commitment =
            Dict.get (Uuid.toString f.uuid) s.state.commitments
    in
    ( { route = f.route
      , what = mainTType
      , uuid = f.uuid
      , type_ = Maybe.andThen third (Dict.get (Uuid.toString f.uuid) s.state.types)
      , provider = Maybe.map .provider commitment
      , receiver = Maybe.map .receiver commitment
      , qty = Maybe.map .qty commitment
      , flow = Maybe.map .flow commitment
      , groups =
            s.state.grouped
                |> Dict.filter (\_ link -> link.groupable == f.uuid)
                |> Dict.values
                |> List.map (\link -> link.group)
      , when = Maybe.map .when commitment |> Maybe.withDefault (millisToPosix 0)
      }
    , Effect.batch [ closeMenu f s.menu ]
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Close ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Commitment <| Route.List (Maybe.map Uuid.toString model.type_) )

        Edit ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Commitment <| Route.Edit (Uuid.toString model.uuid) (Maybe.map Uuid.toString model.type_) )


view : Model -> View Msg
view model =
    { title = "Commitment"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    floatingContainer s
        (Just Close)
        "Commitment"
        [ button.primary (Ok Edit) "Edit" ]
        [ Dict.get (Uuid.toString model.uuid) s.state.types
            |> Maybe.andThen (\( _, _, mpuuid ) -> Maybe.map (\puuid -> displayZone s.state SmallcardZone mainHType puuid) mpuuid)
            |> Maybe.withDefault ""
            |> h1
        , h2 ("Date: " ++ DateTime.toString s.zone model.when)
        , h2
            ("What: "
                ++ (model.qty |> Maybe.map (\expr -> exeval s.state { context = ( Type.TType TType.Commitment, model.uuid ) } s.state.values expr |> Result.map Rational.toFloatString |> Result.withDefault "invalid") |> Maybe.withDefault "(none)")
                ++ " "
                ++ (model.flow |> Maybe.map (\f -> displayZone s.state SmallcardZone (Flow.typeOf f) (Flow.uuidOf f)) |> Maybe.withDefault "(none)")
            )
        , h2 ("Provider: " ++ (model.provider |> Maybe.map (displayZone s.state SmallcardZone (Type.TType TType.Agent)) |> Maybe.withDefault "(none)"))
        , h2 ("Receiver: " ++ (model.receiver |> Maybe.map (displayZone s.state SmallcardZone (Type.TType TType.Agent)) |> Maybe.withDefault "(none)"))
        , text <| displayZone s.state SmallcardZone mainTType model.uuid
        , getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers model.what model.uuid model.type_ False
            |> displayIdentifierDict ""
        , h2 "Values:"
        , getValues s.state.types s.state.valueTypes s.state.values model.what model.uuid model.type_ False
            |> displayValueDict s { context = ( Type.TType TType.Commitment, model.uuid ) } "(none)" s.state.values
        , h2 "Groups:"
        , model.groups
            |> List.map (\guuid -> displayZone s.state SmallcardZone (Type.TType TType.Group) guuid)
            |> displayGroupTable "(none)"
        ]
