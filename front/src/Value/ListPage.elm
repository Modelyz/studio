module Value.ListPage exposing (Flags, Model, Msg, match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Payload
import Route exposing (Route, redirect)
import Scope.View
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
        , view = view
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Entity Route.ValueType (Route.List _) ->
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
            , Shared.dispatch s <| Payload.RemovedValueType i
            )

        Add ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.ValueType (Route.Add Nothing Nothing) )

        View vtid ->
            ( model, Route.redirect s.navkey (Route.Entity Route.ValueType (Route.View vtid Nothing)) |> Effect.fromCmd )


view : Model -> View Msg
view =
    always
        { title = "ValueTypes"
        , attributes = []
        , element = viewContent
        , route = Route.Entity Route.ValueType (Route.List Nothing)
        }


viewContent : Shared.Model -> Element Msg
viewContent s =
    flatContainer s
        Nothing
        "ValueTypes"
        [ button.primary (Ok Add) "Add..."
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
                            (row []
                                [ text <| "for " ++ Scope.View.toDisplay s.state vt.scope ]
                            )
                    )
                |> withDefaultContent (p "There are no Value Types yet. Create your first one!")
            )
        ]
