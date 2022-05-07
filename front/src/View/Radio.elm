module View.Radio exposing (view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import REA.Entity exposing (Entity)
import View exposing (color)


type alias Config msg =
    { title : String
    , options : List ( Entity, String )
    , selected : Maybe Entity
    , msg : Entity -> msg
    }


view : Config msg -> Element msg
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
            , Border.color color.blue
            ]
          <|
            el
                [ width fill
                , height fill
                , Border.rounded 4
                , Background.color <|
                    case state of
                        Input.Idle ->
                            color.white

                        Input.Focused ->
                            color.lightGrey

                        Input.Selected ->
                            color.lightBlue
                ]
                none
        , text label
        ]
