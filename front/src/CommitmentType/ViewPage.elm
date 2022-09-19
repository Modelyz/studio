module CommitmentType.ViewPage exposing (..)

import CommitmentType.CommitmentType as CommitmentType exposing (CommitmentType)
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
import Zone.View exposing (display)
import Zone.Zone exposing (Zone(..))


type alias Flags =
    { route : Route
    , uuid : Maybe Uuid
    }


type alias Model =
    { route : Route
    , commitmentType : Maybe CommitmentType
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
        Route.CommitmentTypeView uuid ->
            Just { route = route, uuid = Uuid.fromString uuid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        mcommitmentType =
            f.uuid |> Maybe.andThen (H.find s.state.commitmentTypes)
    in
    ( { route = f.route
      , commitmentType = f.uuid |> Maybe.andThen (H.find s.state.commitmentTypes)
      , groups =
            mcommitmentType
                |> Maybe.map
                    (\commitment ->
                        s.state.grouped
                            |> Dict.filter (\_ v -> commitment.uuid == Groupable.uuid v.groupable)
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
            model.commitmentType
                |> Maybe.map
                    (\at ->
                        ( model, Effect.fromCmd <| redirect s.navkey (Route.CommitmentTypeEdit (Uuid.toString at.uuid)) )
                    )
                |> Maybe.withDefault ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding a Commitment Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    model.commitmentType
        |> Maybe.map
            (\at ->
                let
                    mconfig =
                        model.commitmentType
                            |> Maybe.map .uuid
                            |> Maybe.andThen
                                (\uuid ->
                                    Config.getMostSpecific s.state.commitments s.state.commitmentTypes s.state.configs SmallcardTitle (HasUserType (Type.HType HType.CommitmentType) uuid)
                                )
                in
                floatingContainer s
                    "CommitmentType"
                    [ button.primary Edit "Edit" ]
                    [ h2 "Parent type:"
                    , at.parent
                        |> Maybe.andThen (H.find s.state.commitmentTypes)
                        |> Maybe.map (withIdentifiers s.state.identifiers)
                        |> Maybe.map (display mconfig)
                        |> Maybe.withDefault "(none)"
                        |> text
                    , h2 "Identifiers:"
                    , at
                        |> withIdentifiers s.state.identifiers
                        |> .identifiers
                        |> displayIdentifierDict "(none)"
                    , h2 "Groups:"
                    , model.groups
                        |> Dict.values
                        |> List.map (withIdentifiers s.state.identifiers)
                        |> List.map
                            (\g ->
                                let
                                    config =
                                        Config.getMostSpecific s.state.groups s.state.groupTypes s.state.configs SmallcardTitle (HasUserType (Type.TType TType.Group) g.uuid)
                                in
                                display config g
                            )
                        |> displayGroupTable "(none)"
                    ]
            )
        |> Maybe.withDefault
            (floatingContainer s
                "CommitmentType"
                []
                [ h1 "Not found", text "The current URL does not correspond to anything" ]
            )
