module GroupType.ListPage exposing (Flags, Model, Msg, init, match, page, update, view, viewContent)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Group.View exposing (hGroupsColumn)
import Group.WithGroups exposing (hWithGroups)
import GroupType.GroupType exposing (GroupType)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (hWithIdentifiers)
import Ident.Identifier as Identifier
import Ident.IdentifierType exposing (IdentifierType)
import Message exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirectView)
import Scope.Scope as Scope exposing (Scope(..))
import Shared
import Spa.Page
import Type exposing (Type(..))
import View exposing (..)
import View.Smallcard exposing (hClickableRemovableCard)
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
        Route.Entity Route.GroupType (Route.List _) ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , viewtype = Smallcard
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed uuid ->
            ( model, Shared.dispatch s <| RemovedGroupType uuid )

        Add ->
            ( model, Route.redirectAdd s.navkey model.route |> Effect.fromCmd )

        View uuid ->
            ( model, Route.redirect s.navkey (Route.Entity Route.GroupType (Route.View (Uuid.toString uuid))) |> Effect.fromCmd )

        ChangeView vt ->
            ( { model | viewtype = vt }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Group Types"
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
                "Group Types"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    (s.state.groupTypes
                        |> Dict.map (\_ t -> hWithIdentifiers s.state.groups Dict.empty s.state.identifierTypes s.state.identifiers t)
                        |> Dict.map (\_ t -> hClickableRemovableCard (View t.uuid) (Removed t.uuid) s.state.groups (Dict.map (\_ v -> hWithIdentifiers s.state.groups s.state.groupTypes s.state.identifierTypes s.state.identifiers v) s.state.groupTypes) s.state.configs t)
                        |> Dict.values
                        |> withDefaultContent (p "There are no Group Types yet. Add your first one!")
                    )
                ]

        Table ->
            flatContainer s
                Nothing
                "Group Types"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    [ table [ width fill, Background.color color.table.inner.background ]
                        { data =
                            s.state.groupTypes
                                |> Dict.values
                                |> List.map (hWithIdentifiers s.state.groups s.state.groupTypes s.state.identifierTypes s.state.identifiers)
                                |> List.map (hWithGroups s.state.grouped)
                        , columns =
                            (s.state.identifierTypes
                                |> Dict.values
                                |> List.filter (\it -> Scope.containsScope s.state.groups s.state.groupTypes it.applyTo (HasType (Type.HType HType.GroupType)))
                                |> List.map identifierColumn
                            )
                                ++ [ hGroupsColumn s ]
                        }
                    ]
                ]


identifierColumn : IdentifierType -> Column GroupType msg
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
