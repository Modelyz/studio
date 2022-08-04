module View.FlatSelect exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import View exposing (..)
import View.Style exposing (..)


type alias Model m a =
    { m | flatselect : Maybe a }


type alias Config a msg =
    { all : List a
    , toString : a -> Element msg
    , toDesc : a -> Element msg
    , onInput : Maybe a -> msg
    , label : String
    , explain : Element msg

    --, field : Model m a -> Maybe a
    }


flatselect : Model m a -> Config a msg -> Element msg
flatselect model c =
    column [ alignTop, spacing 10, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
            [ h2 c.label
            , Maybe.map
                (\x ->
                    row [ Background.color color.item.selected ]
                        [ el [ padding 5 ] (c.toString x)
                        , button.secondary (c.onInput Nothing) "×"
                        ]
                )
                model.flatselect
                -- TODO replace with a (c.field model)
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
