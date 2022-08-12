module Contract.ListPage exposing (match, page)

import Contract.Contract exposing (Contract)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Item.Item as Item exposing (Item)
import Message exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect, redirectAdd)
import Search.Criteria as Criteria exposing (Criteria(..))
import Shared
import Spa.Page
import View exposing (..)
import View.Smallcard exposing (viewSmallCard)
import View.Type as ViewType


type alias Model =
    { route : Route
    , search : Criteria Contract
    }


type Msg
    = Removed Contract
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
        Route.ContractList ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route, search = SearchNothing }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed cn ->
            ( model, Shared.dispatch s <| RemovedContract cn.uuid )

        Add ->
            ( model, redirectAdd "add" s.navkey model.route |> Effect.fromCmd )

        Search str ->
            ( { model | search = SearchFull str }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Contracts"
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
                "Contracts"
                [ button.primary Add "Add..."
                ]
                none
                [ wrappedRow
                    [ spacing 10 ]
                    (s.state.contracts
                        |> Dict.values
                        |> List.map
                            (\cn ->
                                viewSmallCard (Removed cn)
                                    (text <| Uuid.toString cn.uuid)
                                    (cn.type_
                                        |> Item.find s.state.contractTypes
                                        |> Maybe.map
                                            (\cnt -> row [] [ text "Type: ", text <| Uuid.toString cnt.uuid ])
                                        |> Maybe.withDefault none
                                    )
                            )
                        |> withDefaultContent (p "There are no Contracts yet. Add your first one!")
                    )
                ]

        ViewType.New ->
            text "New"
