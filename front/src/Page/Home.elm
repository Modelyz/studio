module Page.Home exposing (match, page, view)

import Effect exposing (Effect)
import Element as E exposing (..)
import Page.Navbar as Navbar
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    ()


type alias Flags =
    { route : Route
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init
        , update = update
        , view = view s
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
    ( ()
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update _ model =
    ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Modelyz"
    , attributes = []
    , element = viewContent
    }


viewContent : Element Msg
viewContent =
    column
        []
        [ row []
            [ paragraph []
                [ text "Modelyz"
                ]
            ]
        ]
