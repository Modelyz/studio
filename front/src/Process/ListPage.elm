module Process.ListPage exposing (match, page)

import Process.Process exposing (Process)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Ident.Identifiable exposing (hWithIdentifiers, tWithIdentifiers)
import Item.Item as Item exposing (Item)
import Message exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect, redirectAdd)
import Search.Criteria as Criteria exposing (Criteria(..))
import Shared
import Spa.Page
import View exposing (..)
import View.Smallcard exposing (tClickableRemovableCard)
import View.Type as ViewType


type alias Model =
    { route : Route
    , search : Criteria Process
    }


type Msg
    = Removed Uuid
    | Add
    | View Uuid
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
        Route.ProcessList _->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route, search = SearchNothing }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed uuid ->
            ( model, Shared.dispatch s <| RemovedProcess uuid )

        Add ->
            ( model, redirectAdd "add" s.navkey model.route |> Effect.fromCmd )

        View uuid ->
            ( model, redirectAdd (Uuid.toString uuid) s.navkey model.route |> Effect.fromCmd )

        Search str ->
            ( { model | search = SearchFull str }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Processes"
    , attributes = []
    , element = viewContent model ViewType.Smallcard
    , route = model.route
    }


viewContent : Model -> ViewType.Type -> Shared.Model -> Element Msg
viewContent model vt s =
    case vt of
        ViewType.Smallcard ->
            let
                allTwithIdentifiers =
                    tWithIdentifiers s.state.identifiers s.state.processes

                allHwithIdentifiers =
                    hWithIdentifiers s.state.identifiers s.state.processTypes
            in
            flatContainer s
                "Processes"
                [ button.primary Add "Add..."
                ]
                none
                [ wrappedRow
                    [ spacing 10 ]
                    (allTwithIdentifiers
                        |> Dict.values
                        |> List.map (\t -> tClickableRemovableCard (View t.uuid) (Removed t.uuid) allTwithIdentifiers allHwithIdentifiers s.state.configs t)
                        |> withDefaultContent (p "There are no Processes yet. Add your first one!")
                    )
                ]

        ViewType.New ->
            text "New"
