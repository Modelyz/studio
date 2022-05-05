module Page.Error exposing (view)

import Html exposing (div, text)
import Page.Navbar as Navbar
import Route exposing (Route)
import Shared
import View exposing (View)


type alias Model =
    { route : Route }


type alias Msg =
    ()


viewContent : Model -> Html.Html Msg
viewContent _ =
    div [] [ text "Error" ]


view : Shared.Model -> Model -> View Msg
view s m =
    { title = "Error"
    , attributes = []
    , element =
        div []
            [ Navbar.view s m.route
            , viewContent m
            ]
    }
