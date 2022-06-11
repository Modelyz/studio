module Page.IdentifierTypes exposing (match, page, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import Prng.Uuid as Uuid
import REA.Entity as Entity exposing (Entity, toPluralString, toUuid)
import REA.Ident as Ident exposing (Fragment(..), Identified(..), IdentifierType, toDesc)
import Result exposing (andThen)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.Navbar as Navbar
import View.Radio as Radio


type alias Model =
    { route : Route }


type Msg
    = Removed IdentifierType
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
        Route.IdentifierTypes ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed i ->
            ( model
            , Shared.dispatch s <| Event.IdentifierTypeRemoved i
            )

        Add ->
            ( model, redirect s.navkey Route.AddIdentifierType |> Effect.fromCmd )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "IdentifierTypes"
    , attributes = []
    , element = viewContent model
    , route = Route.IdentifierTypes
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    flatContent s
        "IdentifierTypes"
        [ button.primary Add "Add..."
        ]
        [ wrappedRow
            [ spacing 10 ]
            (s.state.identifierTypes
                |> Set.toList
                |> List.sortBy .name
                |> List.map
                    (\i ->
                        let
                            ids =
                                Set.toList i.applyTo
                        in
                        viewSmallCard (Removed i)
                            Nothing
                            i.name
                            (if List.isEmpty ids then
                                "for everything"

                             else
                                "for " ++ (ids |> List.map toDesc |> String.join ", ")
                            )
                    )
                |> withDefaultContent (p "There are no Identifier Types yet. Create your first one!")
            )
        ]
