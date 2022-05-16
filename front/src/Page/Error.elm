module Page.Error exposing (view)

import Element exposing (..)
import Route exposing (Route)
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    ()


viewContent : Model -> Element Msg
viewContent _ =
    row [] [ text "Error" ]


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Error"
    , attributes = []
    , element = \_ -> viewContent model
    , route = model.route
    }
