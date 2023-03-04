module Zone.ViewPage exposing (Flags, Model, Msg(..), match, page)

import Configuration exposing (Configuration)
import Configuration.View
import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import View exposing (..)


type alias Flags =
    { route : Route
    , zid : String
    }


type alias Model =
    { route : Route
    , config : Maybe Configuration
    }


type Msg
    = Edit
    | Close


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
        Route.Entity Route.Configuration (Route.View zid _) ->
            Just { route = route, zid = zid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , config = s.state.configs |> Dict.filter (\k _ -> k == f.zid) |> Dict.values |> List.head
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Close ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Configuration (Route.List Nothing) )

        Edit ->
            model.config
                |> Maybe.map
                    (\c ->
                        ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Configuration <| Route.Edit (Configuration.compare c) Nothing )
                    )
                |> Maybe.withDefault ( model, Effect.none )


view : Model -> View Msg
view model =
    { title = "Adding a Configuration"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    model.config
        |> Maybe.map
            (\c ->
                floatingContainer s
                    (Just Close)
                    "Configuration"
                    [ button.primary (Ok Edit) "Edit" ]
                    [ h1 <| Configuration.View.description s c
                    , text <| Configuration.View.view c
                    ]
            )
        |> Maybe.withDefault
            (floatingContainer s
                (Just Close)
                "Configuration"
                []
                [ h1 "Not found", text "The current URL does not correspond to anything" ]
            )
