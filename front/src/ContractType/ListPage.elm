module ContractType.ListPage exposing (Flags, Model, Msg, match, page)

import ContractType.ContractType exposing (ContractType)
import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Group.WithGroups exposing (withGroups)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (withIdentifiers)
import Ident.Identifier as Identifier
import Ident.IdentifierType exposing (IdentifierType)
import Message exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirectAdd)
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
        Route.ContractTypeList ->
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
            ( model, Shared.dispatch s <| RemovedContractType uuid )

        Add ->
            ( model, redirectAdd "add" s.navkey model.route |> Effect.fromCmd )

        View uuid ->
            ( model, redirectAdd (Uuid.toString uuid) s.navkey model.route |> Effect.fromCmd )

        ChangeView vt ->
            ( { model | viewtype = vt }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Contract Types"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    case model.viewtype of
        Smallcard ->
            flatContainer s
                "Contract Types"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    (s.state.contractTypes
                        |> Dict.values
                        |> List.map (withIdentifiers s.state.identifiers)
                        |> List.map (\h -> hClickableRemovableCard (View h.uuid) (Removed h.uuid) s.state.contracts s.state.contractTypes s.state.configs h)
                        |> withDefaultContent (p "There are no Contract Types yet. Add your first one!")
                    )
                ]

        Table ->
            flatContainer s
                "Contract Types"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    [ table [ width fill, Background.color color.table.inner.background ]
                        { data =
                            s.state.contractTypes
                                |> Dict.values
                                |> List.map (\h -> withIdentifiers s.state.identifiers h)
                                |> List.map (\h -> withGroups s.state.grouped h)
                        , columns =
                            (s.state.identifierTypes
                                |> Dict.values
                                |> List.filter (\it -> Scope.containsScope s.state.contracts s.state.contractTypes it.applyTo (HasType (Type.HType HType.ContractType)))
                                |> List.map identifierColumn
                            )
                                ++ [ groupsColumn s ]
                        }
                    ]
                ]


groupsColumn : Shared.Model -> Column ContractType msg
groupsColumn s =
    { header = headerCell color.table.header.background2 "Groups"
    , width = fill
    , view =
        withGroups s.state.grouped
            >> .groups
            >> Dict.values
            >> List.map (withIdentifiers s.state.identifiers)
            >> List.map (tWithDisplay s.state.groups s.state.groupTypes s.state.configs SmallcardTitle)
            >> List.map .display
            >> List.map (Dict.get "SmallcardTitle" >> Maybe.withDefault "(missing zone config)")
            >> String.join ", "
            >> text
            >> el [ height fill, padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ]
    }


identifierColumn : IdentifierType -> Column ContractType msg
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
