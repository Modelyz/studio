module Ident.ListPage exposing (Flags, Model, Msg, match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Ident.IdentifierType as IT exposing (IdentifierType)
import Payload
import Route exposing (Route, redirect)
import Scope.View
import Shared
import Spa.Page
import View exposing (..)
import View.Smallcard exposing (clickableRemovableCard)


type alias Model =
    { route : Route }


type Msg
    = Removed IdentifierType
    | Add
    | View String


type alias Flags =
    { route : Route }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init s
        , update = update s
        , view = view
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Entity Route.IdentifierType (Route.List _) ->
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
            , Shared.dispatch s <| Payload.RemovedIdentifierType i
            )

        Add ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.IdentifierType (Route.Add Nothing Nothing) )

        View vtid ->
            ( model, Route.redirect s.navkey (Route.Entity Route.IdentifierType (Route.View vtid Nothing)) |> Effect.fromCmd )


view : Model -> View Msg
view =
    always
        { title = "Identifier Types"
        , attributes = []
        , element = viewContent
        , route = Route.Entity Route.IdentifierType (Route.List Nothing)
        }


viewContent : Shared.Model -> Element Msg
viewContent s =
    flatContainer s
        Nothing
        "Identifier Types"
        [ button.primary (Ok Add) "Add..."
        ]
        none
        none
        [ wrappedRow
            [ spacing 10 ]
            (s.state.identifierTypes
                |> Dict.values
                |> List.sortBy .name
                |> List.map
                    (\it ->
                        clickableRemovableCard (View <| IT.compare it)
                            (Removed it)
                            (text it.name)
                            (row []
                                [ text <| "for " ++ Scope.View.toDisplay s.state it.scope ]
                            )
                    )
                |> withDefaultContent (p "There are no Identifier Types yet. Create your first one!")
            )
        ]
