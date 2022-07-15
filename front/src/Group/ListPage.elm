module Group.ListPage exposing (match, page, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Entity.Entity as Entity exposing (Entity, only, toPluralString, toUuid)
import Group.Group as Group exposing (Group)
import Html.Attributes as Attr
import Ident.EntityIdentifier as EntityIdentifier
import Ident.View exposing (displayIdentifiers, selectIdentifiers)
import Message
import Navbar
import Prng.Uuid as Uuid
import Result exposing (andThen)
import Route exposing (Route, redirect)
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
        Route.Groups ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed e ->
            ( model
            , Shared.dispatch s <| Message.Removed e
            )

        Add ->
            ( model, redirect s.navkey Route.AddGroup |> Effect.fromCmd )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Groups"
    , attributes = []
    , element = viewContent model
    , route = Route.Groups
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    flatContent s
        "Groups"
        [ button.primary Add "Add..."
        ]
        [ wrappedRow
            [ spacing 10 ]
            (s.state.entities
                |> only "Group"
                |> Set.toList
                |> List.sortBy Entity.compare
                |> List.map
                    (\e ->
                        viewSmallCard (Removed e)
                            Nothing
                            (EntityIdentifier.restrict e s.state.identifiers
                                |> selectIdentifiers Smallcard
                                |> displayIdentifiers
                            )
                            ""
                    )
                |> withDefaultContent (p "There are no Groups yet. Create your first one!")
            )
        ]
