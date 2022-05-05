module Page.NotFound exposing (view, viewContent)

import Browser
import Html exposing (Html, div, text)
import Page.Navbar as Navbar
import Route exposing (Route)
import Shared


type alias Model =
    { route : Route }


type alias Msg =
    ()


view : Shared.Model -> Model -> Browser.Document Msg
view s model =
    { title = "Not Found"
    , body =
        [ Navbar.view s model.route
        , viewContent
        ]
    }


viewContent : Html msg
viewContent =
    div [] [ text "Not Found" ]
