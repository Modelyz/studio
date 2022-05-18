module View.Radio exposing (view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Style exposing (color, size)


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
