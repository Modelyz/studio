module Value.ListPage exposing (Flags, Model, Msg, match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Message
import Route exposing (Route, redirect, redirectViewItem)
import Scope.Scope as Scope
import Shared
import Spa.Page
import Value.ValueType as VT exposing (ValueType)
import View exposing (..)
import View.Smallcard exposing (clickableRemovableCard)


type alias Model =
    { route : Route }


type Msg
    = Removed ValueType
    | Add
    | View String


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
        Route.ValueTypeList ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed i ->
            ( model
            , Shared.dispatch s <| Message.ValueTypeRemoved i
            )

        Add ->
            ( model, redirect s.navkey Route.ValueTypeAdd |> Effect.fromCmd )

        View vtid ->
            ( model, redirectViewItem "view" vtid s.navkey model.route |> Effect.fromCmd )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "ValueTypes"
    , attributes = []
    , element = viewContent model
    , route = Route.ValueTypeList
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    flatContainer s
        "ValueTypes"
        [ button.primary Add "Add..."
        ]
        none
        none
        [ wrappedRow
            [ spacing 10 ]
            (s.state.valueTypes
                |> Dict.values
                |> List.sortBy .name
                |> List.map
                    (\vt ->
                        clickableRemovableCard (View <| VT.compare vt)
                            (Removed vt)
                            (text vt.name)
                            (row [] [ text <| "for ", text <| Scope.toString vt.scope ])
                    )
                |> withDefaultContent (p "There are no Value Types yet. Create your first one!")
            )
        ]
