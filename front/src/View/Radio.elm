module View.Radio exposing (view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import View exposing (color)


type alias Config a msg =
    { title : String
    , options : List ( a, String )
    , selected : Maybe a
    , msg : a -> msg
    }


view : Config a msg -> Element msg
view config =
    column [ spacing 50 ]
        [ Input.radio
            [ padding 10
            , spacing 30
            ]
            { onChange = config.msg
            , selected = config.selected
            , label = Input.labelAbove [] <| text config.title
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
                            color.background.main

                        Input.Focused ->
                            color.background.focus

                        Input.Selected ->
                            color.background.selected
                ]
                none
        , text label
        ]
