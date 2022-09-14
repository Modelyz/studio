module Agent.ListPage exposing (match, page)

import Agent.Agent exposing (Agent)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Ident.Identifiable exposing (hWithIdentifiers, tWithIdentifiers)
import Ident.View exposing (tableColumn)
import Item.Item as Item exposing (Item)
import Message exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect, redirectAdd)
import Scope.Scope as Scope exposing (Scope(..))
import Search.Criteria as Criteria exposing (Criteria(..))
import Shared
import Spa.Page
import Type exposing (Type(..))
import Typed.Type as TType
import View exposing (..)
import View.Smallcard exposing (tClickableRemovableCard)
import View.Style exposing (..)
import View.Type as ViewType exposing (Type(..))


type alias Model =
    { route : Route
    , viewtype : ViewType.Type
    , search : Criteria Agent
    }


type Msg
    = Removed Uuid
    | Add
    | View Uuid
    | ChangeView ViewType.Type
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
        Route.AgentList ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route, viewtype = Smallcard, search = SearchNothing }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed uuid ->
            ( model, Shared.dispatch s <| RemovedAgent uuid )

        Add ->
            ( model, redirectAdd "add" s.navkey model.route |> Effect.fromCmd )

        View uuid ->
            ( model, redirectAdd (Uuid.toString uuid) s.navkey model.route |> Effect.fromCmd )

        ChangeView vt ->
            ( { model | viewtype = vt }, Effect.none )

        Search str ->
            ( { model | search = SearchFull str }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Agents"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    case model.viewtype of
        Smallcard ->
            let
                allTwithIdentifiers =
                    tWithIdentifiers s.state.identifiers s.state.agents

                allHwithIdentifiers =
                    hWithIdentifiers s.state.identifiers s.state.agentTypes
            in
            flatContainer s
                "Agents"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    (allTwithIdentifiers
                        |> Dict.values
                        |> List.map (\t -> tClickableRemovableCard (View t.uuid) (Removed t.uuid) allTwithIdentifiers allHwithIdentifiers s.state.configs t)
                        |> withDefaultContent (p "There are no Agents yet. Add your first one!")
                    )
                ]

        Table ->
            let
                allTwithIdentifiers =
                    tWithIdentifiers s.state.identifiers s.state.agents

                allHwithIdentifiers =
                    hWithIdentifiers s.state.identifiers s.state.agentTypes
            in
            flatContainer s
                "Agents"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    [ table [ width fill, Background.color color.table.inner.background ]
                        { data = Dict.values allTwithIdentifiers
                        , columns =
                            List.map tableColumn <| List.filter (\it -> Scope.containsScope s.state.agents s.state.agentTypes it.applyTo (HasType (Type.TType TType.Agent))) <| Dict.values s.state.identifierTypes
                        }
                    ]
                ]
