module ResourceType.ListPage exposing (match, page)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Ident.Identifiable exposing (hWithIdentifiers)
import Item.Item as Item exposing (Item)
import Message exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import ResourceType.ResourceType exposing (ResourceType)
import Route exposing (Route, redirect, redirectAdd)
import Search.Criteria as Criteria exposing (Criteria(..))
import Shared
import Spa.Page
import View exposing (..)
import View.Smallcard exposing (hViewSmallCard)
import View.Type as ViewType


type alias Model =
    { route : Route
    , search : Criteria ResourceType
    }


type Msg
    = Removed Uuid
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
        Route.ResourceTypeList ->
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
            ( model, Shared.dispatch s <| RemovedResourceType uuid )

        Add ->
            ( model, redirectAdd "add" s.navkey model.route |> Effect.fromCmd )

        Search str ->
            ( { model | search = SearchFull str }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Resource Types"
    , attributes = []
    , element = viewContent model ViewType.Smallcard
    , route = model.route
    }


viewContent : Model -> ViewType.Type -> Shared.Model -> Element Msg
viewContent model vt s =
    case vt of
        ViewType.Smallcard ->
            let
                allHwithIdentifiers =
                    hWithIdentifiers s.state.identifiers s.state.resourceTypes
            in
            flatContainer s
                "Resource Types"
                [ button.primary Add "Add..."
                ]
                none
                [ wrappedRow
                    [ spacing 10 ]
                    (allHwithIdentifiers
                        |> Dict.values
                        |> List.map (\h -> hViewSmallCard (Removed h.uuid) s.state.resources allHwithIdentifiers s.state.configs h)
                        |> withDefaultContent (p "There are no Resource Types yet. Add your first one!")
                    )
                ]

        ViewType.New ->
            text "New"
