module Page.Config exposing (..)

import Browser exposing (Document)
import ES
import Msg exposing (Msg(..))
import Page.Navbar as Navbar


type alias Model =
    ES.State


view : Model -> Document Msg
view model =
    { title = "Processes"
    , body =
        [ Navbar.view
        , viewContent model
        ]
    }
