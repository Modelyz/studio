module Value.ViewPage exposing (Flags, Model, Msg(..), match, page)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Ident.Identifiable exposing (withIdentifiers)
import Route exposing (Route, redirect)
import Scope.View
import Shared
import Spa.Page
import Value.Value as Value exposing (Value)
import Value.ValueType as VT exposing (ValueType)
import View exposing (..)


type alias Flags =
    { route : Route
    , vtid : String
    }


type alias Model =
    { route : Route
    , valueType : Maybe ValueType
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
        Route.Entity Route.ValueType (Route.View vtid) ->
            Just { route = route, vtid = vtid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , valueType = s.state.valueTypes |> Dict.filter (\k _ -> k == f.vtid) |> Dict.values |> List.head
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Close ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.ValueType (Route.List Nothing) )

        Edit ->
            model.valueType
                |> Maybe.map
                    (\vt ->
                        ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.ValueType (Route.Edit (VT.compare vt)) )
                    )
                |> Maybe.withDefault ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Value Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    model.valueType
        |> Maybe.map
            (\vt ->
                floatingContainer s
                    (Just Close)
                    "ValueType"
                    [ button.primary Edit "Edit" ]
                    [ h2 <| vt.name
                    , text <|
                        "Scope: "
                            ++ Scope.View.toDisplay s.state.types s.state.identifiers s.state.configs vt.scope
                    ]
            )
        |> Maybe.withDefault
            (floatingContainer s
                (Just Close)
                "ValueType"
                []
                [ h1 "Not found", text "The current URL does not correspond to anything" ]
            )
