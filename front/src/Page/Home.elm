module Page.Home exposing (match, page, view)

import Effect exposing (Effect)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Page.Navbar as Navbar
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


type alias Model =
    { route : Route }


type alias Msg =
    ()


type alias Flags =
    { route : Route
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init
        , update = update
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Home ->
            Just { route = route }

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init flags =
    ( { route = flags.route
      }
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update _ model =
    ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Modelyz"
    , attributes = []
    , element =
        div []
            [ Navbar.view shared model.route
            , viewContent
            ]
    }


viewContent : Html Msg
viewContent =
    div
        []
        [ div
            [ class "hero is-medium"
            ]
            [ div [ class "hero-body" ]
                [ p [ class "title" ]
                    [ text "Modelyz"
                    ]
                ]
            ]
        ]
