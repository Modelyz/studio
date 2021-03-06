module View.FlatSelect exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Style exposing (..)
import View exposing (..)


type alias Model m a =
    { m | flatselect : Maybe a }


type alias Config a msg =
    { all : List a
    , toString : a -> String
    , toDesc : a -> Maybe String
    , onInput : Maybe a -> msg
    , label : String
    , explain : Element msg
    }


flatselect : Model m a -> Config a msg -> Element msg
flatselect model c =
    column [ alignTop, spacing 10, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
            [ h2 c.label
            , Maybe.map
                (\x ->
                    row [ Background.color color.item.selected ]
                        [ el [ padding 5 ] (text <| c.toString x)
                        , button.secondary (c.onInput Nothing) "×"
                        ]
                )
                model.flatselect
                |> Maybe.withDefault
                    (el
                        [ padding 5, Font.color color.text.disabled ]
                        (text "Empty")
                    )
            ]
        , c.explain
        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ] <|
            List.map
                (\x -> clickableCard (c.onInput <| Just x) (c.toString x) (c.toDesc x))
                c.all
        ]
