module ProcessType.ListPage exposing (match, page)

import ProcessType.ProcessType exposing (ProcessType)
import Configuration as Config
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable
import Group.Link as GroupLink exposing (groupsOf)
import Group.WithGroups exposing (WithGroups)
import Hierarchy.Type as HType
import Ident.Identifiable as Identifiable exposing (hWithIdentifiers, withIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (IdentifierType)
import Item.Item as Item exposing (Item)
import Message exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect, redirectAdd)
import Scope.Scope as Scope exposing (Scope(..))
import Shared
import Spa.Page
import Type exposing (Type(..))
import Typed.Type as TType
import View exposing (..)
import View.Smallcard exposing (hClickableRemovableCard)
import View.Style exposing (..)
import View.Type as ViewType exposing (Type(..))
import Zone.Zone exposing (Zone(..))


type alias Model =
    { route : Route
    , viewtype : ViewType.Type
    }


type alias Record =
    { identifiers : Dict String Identifier
    , grouped : Dict String GroupLink.Link
    }


toRecord : Dict String Identifier -> Dict String GroupLink.Link -> Item a -> Record
toRecord allIds allGroupLinks i =
    { identifiers = allIds |> Dict.filter (\_ v -> v.identifiable == i.uuid)
    , grouped = allGroupLinks |> Dict.filter (\_ v -> Groupable.uuid v.groupable == i.uuid)
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
        Route.ProcessTypeList ->
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
            ( model, Shared.dispatch s <| RemovedProcessType uuid )

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
    { title = "Process Types"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    case model.viewtype of
        Smallcard ->
            flatContainer s
                "Process Types"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    (s.state.processTypes
                        |> Dict.values
                        |> List.map (\h -> hClickableRemovableCard (View h.uuid) (Removed h.uuid) s.state.processes s.state.processTypes s.state.configs h)
                        |> withDefaultContent (p "There are no Process Types yet. Add your first one!")
                    )
                ]

        Table ->
            flatContainer s
                "Process Types"
                [ button.primary Add "Add..."
                ]
                none
                (View.viewSelector [ Smallcard, Table ] model.viewtype ChangeView)
                [ wrappedRow
                    [ spacing 10 ]
                    [ table [ width fill, Background.color color.table.inner.background ]
                        { data = Dict.values s.state.processTypes |> List.map (toRecord s.state.identifiers s.state.grouped)
                        , columns =
                            (s.state.identifierTypes
                                |> Dict.values
                                |> List.filter (\it -> Scope.containsScope s.state.processes s.state.processTypes it.applyTo (HasType (Type.HType HType.ProcessType)))
                                |> List.map identifierColumn
                            )
                                ++ [ groupsColumn s ]
                        }
                    ]
                ]


groupsColumn : Shared.Model -> Column Record msg
groupsColumn s =
    { header = headerCell "Groups"
    , width = fill
    , view =
        .grouped
            >> Dict.values
            >> List.map
                (\gl ->
                    let
                        config =
                            Config.getMostSpecific s.state.groups s.state.groupTypes s.state.configs SmallcardTitle (HasUserType (Type.TType TType.Group) gl.group.uuid)
                    in
                    Identifiable.display config gl.group
                )
            >> String.join ", "
            >> text
    }


identifierColumn : IdentifierType -> Column Record msg
identifierColumn it =
    { header = headerCell it.name
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
