module Entity.ListPage exposing (Config, Flags, Model, Msg, init, update, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Entity.Entity as Entity exposing (Entity, only, toPluralString, toString)
import Html.Attributes as Attr
import Ident.Identifier as Identifier
import Ident.Scope exposing (Scope(..))
import Ident.View exposing (buildDisplayIdentifier, displayIdentifiers)
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Result exposing (andThen)
import Route exposing (Route, redirectAdd)
import Search.Criteria as Criteria exposing (Criteria(..))
import Shared
import Spa.Page
import View exposing (..)
import View.Navbar as Navbar
import View.Radio as Radio
import View.Smallcard exposing (viewSmallCard)
import View.Style exposing (..)
import View.Type as ViewType exposing (Type(..))


type alias Model =
    { route : Route
    , search : Criteria
    }


type Msg
    = Removed Entity
    | Add
    | Search Entity String


type alias Flags =
    { route : Route }


type alias Config =
    { pageTitle : String
    , entityType : String
    , emptyText : String
    }


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route, search = SearchNothing }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed a ->
            ( model, Shared.dispatch s <| Message.Removed a )

        Add ->
            ( model, redirectAdd "add" s.navkey model.route |> Effect.fromCmd )

        Search e str ->
            ( { model | search = SearchFull e str }, Effect.none )


view : Config -> Shared.Model -> Model -> View Msg
view c s model =
    { title = c.pageTitle
    , attributes = []
    , element = viewContent c model Smallcard
    , route = model.route
    }


viewContent : Config -> Model -> ViewType.Type -> Shared.Model -> Element Msg
viewContent c model vt s =
    let
        entities =
            only c.entityType s.state.entities

        --TODO |> Criteria.entitySearch model.search
    in
    case vt of
        Smallcard ->
            flatContainer s
                c.pageTitle
                [ button.primary Add "Add..."
                ]
                none
                [ wrappedRow
                    [ spacing 10 ]
                    (entities
                        |> Set.toList
                        |> List.map
                            (\e ->
                                viewSmallCard (Removed e)
                                    Nothing
                                    (Identifier.restrict e s.state.identifiers
                                        |> buildDisplayIdentifier Smallcard e
                                        -- TODO merge build and show?
                                        |> displayIdentifiers (Entity.toUuidString e)
                                    )
                                    (Entity.toTypeUuid e
                                        |> Maybe.map (\u -> "Type: " ++ Uuid.toString u)
                                    )
                            )
                        |> withDefaultContent (p c.emptyText)
                    )
                ]

        New ->
            text "New"
