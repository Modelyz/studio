module Contract.ListPage exposing (Flags, Model, Msg, match, page)

import Contract.Contract exposing (Contract)
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
import Route exposing (Route, redirectView)
import Scope.Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
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
        Route.Entity Route.Contract (Route.List _) ->
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
            ( model, Shared.dispatch s <| RemovedContract uuid )

        Add ->
            ( model, Route.redirectAdd s.navkey model.route |> Effect.fromCmd )

        View uuid ->
            ( model, Route.redirect s.navkey (Route.Entity Route.Contract (Route.View (Uuid.toString uuid))) |> Effect.fromCmd )

        ChangeView vt ->
            ( { model | viewtype = vt }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Contracts"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        allT =
            s.state.contracts |> Dict.map (\_ t -> tWithIdentifiers s.state.contracts Dict.empty s.state.identifierTypes s.state.identifiers t)

        allH =
            s.state.contractTypes |> Dict.map (\_ v -> hWithIdentifiers s.state.contracts s.state.contractTypes s.state.identifierTypes s.state.identifiers v)
    in
    case model.viewtype of
        Smallcard ->
            flatContainer s
                Nothing
                "Contracts"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    (allT
                        |> Dict.map (\_ t -> tClickableRemovableCard (View t.uuid) (Removed t.uuid) s.state.contracts allH s.state.configs t)
                        |> Dict.values
                        |> withDefaultContent (p "There are no Contracts yet. Add your first one!")
                    )
                ]

        Table ->
            flatContainer s
                Nothing
                "Contracts"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    [ table [ width fill, Background.color color.table.inner.background ]
                        { data =
                            allT
                                |> Dict.values
                                |> List.map (tWithGroups s.state.grouped)
                        , columns =
                            (s.state.identifierTypes
                                |> Dict.values
                                |> List.filter (\it -> containsScope s.state.contracts s.state.contractTypes it.scope (HasType (Type.TType TType.Contract)))
                                |> List.map identifierColumn
                            )
                                ++ [ tGroupsColumn s ]
                        }
                    ]
                ]


identifierColumn : IdentifierType -> Column Contract msg
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
