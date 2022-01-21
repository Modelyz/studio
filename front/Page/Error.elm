module Page.Error exposing (view)

import Browser
import Html exposing (Html, a, div, img, nav, span, text)
import Html.Attributes exposing (attribute, class, href, src, width)
import ES
import Page.Navbar as Navbar
import Msg exposing (Msg(..))

type alias Model =
    ES.State


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
