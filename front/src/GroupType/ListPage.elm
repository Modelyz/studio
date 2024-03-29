module GroupType.ListPage exposing (Flags, Model, Msg, match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Hierarchy.Type as HType
import Payload exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route)
import Scope exposing (Scope(..))
import Shared
import Spa.Page
import Type
import View exposing (..)
import View.Smallcard exposing (tClickableRemovableCard)
import View.Style exposing (..)
import View.Table exposing (hView)
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
        , view = view
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Entity Route.GroupType (Route.List _) ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , viewtype = ViewType.Smallcard
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed uuid ->
            ( model, Shared.dispatch s <| RemovedGroupType uuid )

        Add ->
            ( model, Route.redirectAdd s.navkey model.route |> Effect.fromCmd )

        View uuid ->
            ( model, Route.redirect s.navkey (Route.Entity Route.GroupType (Route.View {uuid = Uuid.toString uuid, type_ = Nothing})) |> Effect.fromCmd )

        ChangeView vt ->
            ( { model | viewtype = vt }, Effect.none )


view : Model -> View Msg
view model =
    { title = "Group Types"
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
                "Group Types"
                [ button.primary (Ok Add) "Add..."
                ]
                none
                (View.viewSelector [ ViewType.Smallcard, ViewType.Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    (s.state.groupTypes
                        |> Dict.map (\_ t -> tClickableRemovableCard s.state (View t.uuid) (Removed t.uuid) (Type.HType t.what) t.uuid)
                        |> Dict.values
                        |> withDefaultContent (p "There are no Group Types yet. Add your first one!")
                    )
                ]

        ViewType.Table ->
            flatContainer s
                Nothing
                "Group Types"
                [ button.primary (Ok Add) "Add..."
                ]
                none
                (View.viewSelector [ ViewType.Smallcard, ViewType.Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    [ hView s.state (HasType (Type.HType HType.GroupType)) (Dict.values s.state.resourceTypes)
                    ]
                ]
