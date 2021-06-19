module NotFound exposing (..)

import Browser
import Html exposing (div, text)
import Msg 

document: Browser.Document Msg.Msg
document =
    { title = "Not Found"
    , body =
      [ div [][ text "Not Found" ] ]
    }
