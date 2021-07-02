module Page.Error exposing (..)

import Msg
import Html exposing (Html, div, text)

document: Html.Html Msg.Msg
document =
    div [][ text "Error" ]
