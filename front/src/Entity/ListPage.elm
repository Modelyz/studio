module Entity.ListPage exposing (Config, Flags, Model, Msg, init, update, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Entity.Entity as Entity exposing (Entity, toPluralString)
import Html.Attributes as Attr
import Ident.EntityIdentifier as EntityIdentifier
import Ident.Identifiable as Identifiable
import Ident.Identifier as Identifier
import Ident.Scope exposing (Scope(..))
import Ident.View exposing (displayIdentifiers, selectIdentifiers)
import Message
import Navbar
import Prng.Uuid as Uuid exposing (Uuid)
import Result exposing (andThen)
import Route exposing (Route, redirectAdd)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.Radio as Radio


type alias Model =
    { route : Route }


type Msg
    = Removed Entity
    | Add


type alias Flags =
    { route : Route }


type alias Config =
    { pageTitle : String
    , entityType : String
    , emptyText : String
    }


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed a ->
            ( model
            , Shared.dispatch s <| Message.Removed a
            )

        Add ->
            ( model, redirectAdd "add" s.navkey model.route |> Effect.fromCmd )


view : Config -> Shared.Model -> Model -> View Msg
view c s model =
    { title = c.pageTitle
    , attributes = []
    , element = viewContent c model Smallcard
    , route = model.route
    }


viewContent : Config -> Model -> ViewType -> Shared.Model -> Element Msg
viewContent c model vt s =
    case vt of
        Smallcard ->
            flatContent s
                c.pageTitle
                [ button.primary Add "Add..."
                ]
                [ wrappedRow
                    [ spacing 10 ]
                    (s.state.entities
                        |> Set.toList
                        |> List.map
                            (\e ->
                                viewSmallCard (Removed e)
                                    Nothing
                                    (EntityIdentifier.restrict e s.state.identifiers
                                        |> selectIdentifiers Smallcard
                                        |> displayIdentifiers
                                    )
                                    ("Type: " ++ Entity.toType e)
                            )
                        |> withDefaultContent (p c.emptyText)
                    )
                ]

        New ->
            text "New"
