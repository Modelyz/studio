module Page.NotFound exposing (view)

import Browser
import Html exposing (Html, div, text)
import Msg exposing (Msg(..))
import Page.Navbar as Navbar
import State exposing (State)


type alias Model =
    State


view : Model -> Browser.Document Msg
view model =
    { title = "Not Found"
    , body =
        [ Navbar.view model
        , viewContent
        ]
    }


viewContent : Html msg
viewContent =
    div [] [ text "Not Found" ]
