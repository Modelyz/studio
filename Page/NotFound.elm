module Page.NotFound exposing (view)

import Browser
import Html exposing (Html, a, div, img, nav, span, text)
import Html.Attributes exposing (attribute, class, href, src, width)
import Page.Navbar as Navbar
import ES
import Msg exposing (Msg(..))

type alias Model =
    ES.State


view : Model -> Browser.Document Msg
view model =
    { title = "Not Found"
    , body =
        [ Navbar.view model
        , viewContent model
        ]
    }


viewContent : Model -> Html.Html msg
viewContent model =
    div [] [ text "Not Found" ]
