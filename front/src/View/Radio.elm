module View.Radio exposing (Config, view)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import View.Style exposing (size)


type alias Config a msg =
    { title : String
    , options : List ( a, String )
    , selected : Maybe a
    , msg : a -> msg
    }


view : Config a msg -> Element msg
view config =
    Input.radio
        [ spacing 10
        , Font.size size.text.main
        ]
        { onChange = config.msg
        , selected = config.selected
        , label = Input.labelAbove [ paddingXY 0 20 ] <| text config.title
        , options = List.map (\( o, t ) -> Input.option o <| text t) config.options
        }
