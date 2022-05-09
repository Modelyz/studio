module View.Radio exposing (view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import View exposing (color, size)


type alias Config a msg =
    { title : String
    , options : List ( a, String )
    , selected : Maybe a
    , msg : a -> msg
    }


view : Config a msg -> Element msg
view config =
    column []
        [ Input.radio
            [ spacing 5
            , Font.size size.text.main
            ]
            { onChange = config.msg
            , selected = config.selected
            , label = Input.labelAbove [ paddingXY 0 10 ] <| text config.title
            , options = List.map (\( o, t ) -> Input.optionWith o <| radioOption t) config.options
            }
        ]


radioOption : String -> Input.OptionState -> Element msg
radioOption label state =
    row [ spacing 10 ]
        [ el
            [ width (px 30)
            , height (px 30)
            , centerY
            , padding 4
            , Border.rounded 6
            , Border.width 2
            , Border.color color.border.main
            ]
          <|
            el
                [ width fill
                , height fill
                , Border.rounded 4
                , Background.color <|
                    case state of
                        Input.Idle ->
                            color.widget.background

                        Input.Focused ->
                            color.item.background

                        Input.Selected ->
                            color.widget.selected
                ]
                none
        , text label
        ]
