module Resource.ListPage exposing (match, page)

import Resource.Resource exposing (Resource)
import Configuration as Config
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Group.WithGroups as WithGroups exposing (withGroups)
import Ident.Identifiable as Identifiable exposing (hWithIdentifiers, tWithIdentifiers, withIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (IdentifierType)
import Message exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect, redirectAdd)
import Scope.Scope as Scope exposing (Scope(..))
import Shared
import Spa.Page
import Type exposing (Type(..))
import Typed.Type as TType
import View exposing (..)
import View.Smallcard exposing (tClickableRemovableCard)
import View.Style exposing (..)
import View.Type as ViewType exposing (Type(..))
import Zone.View exposing (display)
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
        Route.ResourceList ->
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
            ( model, Shared.dispatch s <| RemovedResource uuid )

        Add ->
            ( model, redirectAdd "add" s.navkey model.route |> Effect.fromCmd )

        View uuid ->
            ( model, redirectAdd (Uuid.toString uuid) s.navkey model.route |> Effect.fromCmd )

        ChangeView vt ->
            ( { model | viewtype = vt }, Effect.none )

        Search str ->
            ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Resources"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    case model.viewtype of
        Smallcard ->
            flatContainer s
                "Resources"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    (s.state.resources
                        |> Dict.values
                        |> List.map (withIdentifiers s.state.identifiers)
                        |> List.map (\t -> tClickableRemovableCard (View t.uuid) (Removed t.uuid) s.state.resources s.state.resourceTypes s.state.configs t)
                        |> withDefaultContent (p "There are no Resources yet. Add your first one!")
                    )
                ]

        Table ->
            flatContainer s
                "Resources"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    [ table [ width fill, Background.color color.table.inner.background ]
                        { data =
                            s.state.resources
                                |> Dict.values
                                |> List.map (withIdentifiers s.state.identifiers)
                                |> List.map (withGroups s.state.grouped)
                        , columns =
                            (s.state.identifierTypes
                                |> Dict.values
                                |> List.filter (\it -> Scope.containsScope s.state.resources s.state.resourceTypes it.applyTo (HasType (Type.TType TType.Resource)))
                                |> List.map identifierColumn
                            )
                                ++ [ groupsColumn s ]
                        }
                    ]
                ]


groupsColumn : Shared.Model -> Column Resource msg
groupsColumn s =
    { header = headerCell color.table.header.background2 "Groups"
    , width = fill
    , view =
        withGroups s.state.grouped
            >> .groups
            >> Dict.values
            >> List.map (withIdentifiers s.state.identifiers)
            >> List.map
                (\g ->
                    let
                        config =
                            Config.getMostSpecific s.state.groups s.state.groupTypes s.state.configs SmallcardTitle (HasUserType (Type.TType TType.Group) g.uuid)
                    in
                    display config g
                )
            >> String.join ", "
            >> text
            >> el [ height fill, padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ]
    }


identifierColumn : IdentifierType -> Column Resource msg
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
