module Group.ListPage exposing (Flags, Model, Msg, match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Payload exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route)
import Scope exposing (Scope(..))
import Shared
import Spa.Page
import Type
import Typed.Type as TType
import View exposing (..)
import View.Smallcard exposing (tClickableRemovableCard)
import View.Style exposing (..)
import View.Table exposing (tView)
import View.Type as ViewType


type alias Model =
    { route : Route
    , viewtype : ViewType.Type
    , filter : Maybe Uuid
    }


type Msg
    = Removed Uuid
    | Add
    | View Uuid
    | ChangeView ViewType.Type


type alias Flags =
    { route : Route, tuuid : Maybe Uuid }


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
        Route.Entity Route.Group (Route.List tuuid) ->
            Just { route = route, tuuid = Maybe.andThen Uuid.fromString tuuid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route, filter = f.tuuid, viewtype = ViewType.Smallcard }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed uuid ->
            ( model, Shared.dispatch s <| RemovedGroup uuid )

        Add ->
            ( model, Route.redirectAdd s.navkey model.route |> Effect.fromCmd )

        View uuid ->
            ( model, Route.redirect s.navkey (Route.Entity Route.Group (Route.View (Uuid.toString uuid) (Maybe.map Uuid.toString model.filter))) |> Effect.fromCmd )

        ChangeView vt ->
            ( { model | viewtype = vt }, Effect.none )


view : Model -> View Msg
view model =
    { title = "Groups"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        entities =
            s.state.groups
                |> Dict.filter (\_ c -> model.filter |> Maybe.map (Type.isParentOf s.state.types c.type_) |> Maybe.withDefault True)
                |> Dict.values
    in
    case model.viewtype of
        ViewType.Smallcard ->
            flatContainer s
                Nothing
                "Groups"
                [ button.primary (Ok Add) "Add..."
                ]
                none
                (View.viewSelector [ ViewType.Smallcard, ViewType.Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    (entities
                        |> List.map (\t -> tClickableRemovableCard s.state (View t.uuid) (Removed t.uuid) (Type.TType t.what) t.uuid)
                        |> withDefaultContent (p "There are no Groups yet. Add your first one!")
                    )
                ]

        ViewType.Table ->
            let
                t =
                    Type.TType TType.Group

                scope =
                    model.filter |> Maybe.map (HasUserType t) |> Maybe.withDefault (HasType t)
            in
            flatContainer s
                Nothing
                "Groups"
                [ button.primary (Ok Add) "Add..."
                ]
                none
                (View.viewSelector [ ViewType.Smallcard, ViewType.Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    [ tView s.state scope entities
                    ]
                ]
