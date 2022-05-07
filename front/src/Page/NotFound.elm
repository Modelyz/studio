module Page.NotFound exposing (defaultView, view, viewContent)

import Element exposing (..)
import Route exposing (Route)
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    ()


view : Shared.Model -> Model -> View Msg
view _ _ =
    defaultView


defaultView : View msg
defaultView =
    { title = "Not Found"
    , attributes = []
    , element = viewContent
    }


viewContent : Element msg
viewContent =
    row [ centerX, centerY ] [ text "Not Found" ]
