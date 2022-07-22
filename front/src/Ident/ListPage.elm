module Ident.ListPage exposing (match, page, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Ident.Scope as Scope
import Message
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import View exposing (..)
import View.Smallcard exposing (viewSmallCard)


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
        Route.IdentifierTypeList ->
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
            , Shared.dispatch s <| Message.IdentifierTypeRemoved i
            )

        Add ->
            ( model, redirect s.navkey Route.IdentifierTypeAdd |> Effect.fromCmd )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "IdentifierTypes"
    , attributes = []
    , element = viewContent model
    , route = Route.IdentifierTypeList
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    flatContainer s
        "IdentifierTypes"
        [ button.primary Add "Add..."
        ]
        none
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
                            (text i.name)
                            (if List.isEmpty ids then
                                Just "for everything"

                             else
                                Just <| "for " ++ (ids |> List.map Scope.toDesc |> String.join ", ")
                            )
                    )
                |> withDefaultContent (p "There are no Identifier Types yet. Create your first one!")
            )
        ]
