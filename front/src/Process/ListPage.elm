module Process.ListPage exposing (Flags, Model, Msg, match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Group.View exposing (tGroupsColumn)
import Group.WithGroups exposing (tWithGroups)
import Ident.Identifiable exposing (hWithIdentifiers, tWithIdentifiers)
import Ident.Identifier as Identifier
import Ident.IdentifierType exposing (IdentifierType)
import Message exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process exposing (Process)
import Route exposing (Route, redirectView, redirectViewItem)
import Scope.Scope as Scope exposing (Scope(..))
import Shared
import Spa.Page
import Type exposing (Type(..))
import Typed.Type as TType
import View exposing (..)
import View.Smallcard exposing (tClickableRemovableCard)
import View.Style exposing (..)
import View.Type as ViewType exposing (Type(..))
import Zone.View exposing (tWithDisplay)
import Zone.Zone exposing (Zone(..))


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
        Route.ProcessList _ ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route, viewtype = Smallcard }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed uuid ->
            ( model, Shared.dispatch s <| RemovedProcess uuid )

        Add ->
            ( model, redirectView "add" s.navkey model.route |> Effect.fromCmd )

        View uuid ->
            ( model, redirectViewItem "view" (Uuid.toString uuid) s.navkey model.route |> Effect.fromCmd )

        ChangeView vt ->
            ( { model | viewtype = vt }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Processes"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    case model.viewtype of
        Smallcard ->
            flatContainer s
                Nothing
                "Processes"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    (s.state.processes
                        |> Dict.map (\_ t -> tWithIdentifiers s.state.processes Dict.empty s.state.identifierTypes s.state.identifiers t)
                        |> Dict.map (\_ t -> tClickableRemovableCard (View t.uuid) (Removed t.uuid) s.state.processes (Dict.map (\_ v -> hWithIdentifiers s.state.processes s.state.processTypes s.state.identifierTypes s.state.identifiers v) s.state.processTypes) s.state.configs t)
                        |> Dict.values
                        |> withDefaultContent (p "There are no Processes yet. Add your first one!")
                    )
                ]

        Table ->
            flatContainer s
                Nothing
                "Processes"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    [ table [ width fill, Background.color color.table.inner.background ]
                        { data =
                            s.state.processes
                                |> Dict.values
                                |> List.map (tWithIdentifiers s.state.processes s.state.processTypes s.state.identifierTypes s.state.identifiers)
                                |> List.map (tWithGroups s.state.grouped)
                        , columns =
                            (s.state.identifierTypes
                                |> Dict.values
                                |> List.filter (\it -> Scope.containsScope s.state.processes s.state.processTypes it.applyTo (HasType (Type.TType TType.Process)))
                                |> List.map identifierColumn
                            )
                                ++ [ tGroupsColumn s ]
                        }
                    ]
                ]


identifierColumn : IdentifierType -> Column Process msg
identifierColumn it =
    { header = headerCell color.table.header.background it.name
    , width = fill
    , view =
        .identifiers
            >> Dict.values
            >> List.filter (\id -> id.name == it.name)
            >> List.map Identifier.toValue
            >> List.head
            >> Maybe.withDefault ""
            >> innerCell
    }
