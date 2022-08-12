module ProcessType.ListPage exposing (match, page)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Item.Item as Item exposing (Item)
import Message exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import ProcessType.ProcessType exposing (ProcessType)
import Route exposing (Route, redirect, redirectAdd)
import Search.Criteria as Criteria exposing (Criteria(..))
import Shared
import Spa.Page
import View exposing (..)
import View.Smallcard exposing (viewSmallCard)
import View.Type as ViewType


type alias Model =
    { route : Route
    , search : Criteria ProcessType
    }


type Msg
    = Removed ProcessType
    | Add
    | Search String


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
        Route.ProcessTypeList ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route, search = SearchNothing }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed pt ->
            ( model, Shared.dispatch s <| RemovedProcessType pt.uuid )

        Add ->
            ( model, redirectAdd "add" s.navkey model.route |> Effect.fromCmd )

        Search str ->
            ( { model | search = SearchFull str }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "ProcessTypes"
    , attributes = []
    , element = viewContent model ViewType.Smallcard
    , route = model.route
    }


viewContent : Model -> ViewType.Type -> Shared.Model -> Element Msg
viewContent model vt s =
    --TODO |> Criteria.entitySearch model.search
    case vt of
        ViewType.Smallcard ->
            flatContainer s
                "ProcessTypes"
                [ button.primary Add "Add..."
                ]
                none
                [ wrappedRow
                    [ spacing 10 ]
                    (s.state.processTypes
                        |> Dict.values
                        |> List.map
                            (\pt ->
                                viewSmallCard (Removed pt)
                                    (text <| Uuid.toString pt.uuid)
                                    (pt.parent
                                        |> Maybe.andThen (Item.find s.state.processTypes)
                                        |> Maybe.map
                                            (\ptp -> row [] [ text "Type: ", text <| Uuid.toString ptp.uuid ])
                                        |> Maybe.withDefault none
                                    )
                            )
                        |> withDefaultContent (p "There are no ProcessTypes yet. Add your first one!")
                    )
                ]

        ViewType.New ->
            text "New"
