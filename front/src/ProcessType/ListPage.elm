module ProcessType.ListPage exposing (Flags, Model, Msg, match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Group.View exposing (groupsColumn)
import Hierarchy.Type as HType
import Ident.View exposing (identifierColumn)
import Message exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route)
import Scope.Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Shared
import Spa.Page
import Type
import Type.View
import View exposing (..)
import View.Smallcard exposing (tClickableRemovableCard)
import View.Style exposing (..)
import View.Type as ViewType


type alias Model =
    { route : Route
    , viewtype : ViewType.Type
    }


type Msg
    = Removed Uuid
    | Add
    | View Uuid
    | ChangeView ViewType.Type


type alias Flags =
    { route : Route }


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
        Route.Entity Route.ProcessType (Route.List _) ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route, viewtype = ViewType.Smallcard }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed uuid ->
            ( model, Shared.dispatch s <| RemovedProcessType uuid )

        Add ->
            ( model, Route.redirectAdd s.navkey model.route |> Effect.fromCmd )

        View uuid ->
            ( model, Route.redirect s.navkey (Route.Entity Route.ProcessType (Route.View (Uuid.toString uuid) Nothing)) |> Effect.fromCmd )

        ChangeView vt ->
            ( { model | viewtype = vt }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Process Types"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    case model.viewtype of
        ViewType.Smallcard ->
            flatContainer s
                Nothing
                "Process Types"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ ViewType.Smallcard, ViewType.Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    (s.state.processTypes
                        |> Dict.map (\_ t -> tClickableRemovableCard (View t.uuid) (Removed t.uuid) s.state.types s.state.configs s.state.identifiers s.state.grouped (Type.HType t.what) t.uuid)
                        |> Dict.values
                        |> withDefaultContent (p "There are no Process Types yet. Add your first one!")
                    )
                ]

        ViewType.Table ->
            flatContainer s
                Nothing
                "Process Types"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ ViewType.Smallcard, ViewType.Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    [ table [ width fill, Background.color color.table.inner.background ]
                        { data =
                            Dict.values s.state.processTypes
                                |> List.map (\a -> ( a.uuid, Type.HType a.what, a.parent ))
                        , columns =
                            Type.View.typeColumn s
                                :: (s.state.identifierTypes
                                        |> Dict.values
                                        |> List.filter (\it -> containsScope s.state.types it.scope (HasType (Type.HType HType.ProcessType)))
                                        |> List.map (identifierColumn s)
                                   )
                                ++ [ groupsColumn s ]
                        }
                    ]
                ]
