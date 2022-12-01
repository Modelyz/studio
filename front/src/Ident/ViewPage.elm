module Ident.ViewPage exposing (Flags, Model, Msg(..), match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Ident.IdentifierType as VT exposing (IdentifierType)
import Route exposing (Route, redirect)
import Scope.View
import Shared
import Spa.Page
import View exposing (..)


type alias Flags =
    { route : Route
    , vtid : String
    }


type alias Model =
    { route : Route
    , identifierType : Maybe IdentifierType
    }


type Msg
    = Edit
    | Close


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
        Route.Entity Route.IdentifierType (Route.View vtid Nothing) ->
            Just { route = route, vtid = vtid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , identifierType = s.state.identifierTypes |> Dict.filter (\k _ -> k == f.vtid) |> Dict.values |> List.head
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Close ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.IdentifierType <| Route.List Nothing )

        Edit ->
            model.identifierType
                |> Maybe.map
                    (\it ->
                        ( model, Effect.fromCmd <| redirect s.navkey (Route.Entity Route.IdentifierType (Route.Edit (VT.compare it) Nothing)) )
                    )
                |> Maybe.withDefault ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Identifier Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    model.identifierType
        |> Maybe.map
            (\it ->
                floatingContainer s
                    (Just Close)
                    "IdentifierType"
                    [ button.primary Edit "Edit" ]
                    [ h2 <| it.name
                    , text <| "Scope: " ++ Scope.View.toDisplay s it.scope
                    ]
            )
        |> Maybe.withDefault
            (floatingContainer s
                (Just Close)
                "IdentifierType"
                []
                [ h1 "Not found", text "The current URL does not correspond to anything" ]
            )
