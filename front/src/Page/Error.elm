module Page.Error exposing (view)

import Browser
import Html exposing (div, text)
import Msg exposing (Msg(..))
import Page.Navbar as Navbar
import State exposing (State)


type alias Model =
    State


viewContent : Model -> Html.Html Msg
viewContent _ =
    div [] [ text "Error" ]


view : Model -> Browser.Document Msg
view model =
    { title = "Error"
    , body =
        [ Navbar.view model
        , viewContent model
        ]
    }
