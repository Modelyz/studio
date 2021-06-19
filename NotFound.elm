module NotFound exposing (..)

import Html exposing (Html, div, text)
import Msg 

document: Html.Html Msg.Msg
document =
    div [][ text "Not Found" ]
