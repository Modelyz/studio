module Expression.RationalInput exposing (RationalInput, adaptWidth, inputText, mandatory, toString)

import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Expression.Rational as Rational exposing (Rational)
import Html.Attributes as Attr
import View.Style exposing (color)


type alias RationalInput =
    String


toString : RationalInput -> String
toString =
    identity


adaptWidth : RationalInput -> Int
adaptWidth r =
    -- adapt the input form width to the content
    50 + 10 * String.length r


mandatory : Bool -> String -> Result String Rational
mandatory isMandatory str =
    if String.length str == 0 && isMandatory then
        Err "Cannot be zerp"

    else
        Ok Rational.zero


inputText : (String -> Result String a) -> Maybe String -> (RationalInput -> msg) -> RationalInput -> Element msg
inputText validate placeholder onInput input =
    Input.text
        [ width <| px <| adaptWidth input
        , htmlAttribute <| Attr.title <| Rational.parse input
        , Background.color
            (case validate input of
                Ok _ ->
                    color.content.background

                Err "" ->
                    color.content.background

                Err _ ->
                    color.item.warning
            )
        ]
        { onChange = onInput
        , text = input
        , placeholder = placeholder |> Maybe.map (text >> Input.placeholder [])
        , label = Input.labelHidden (Maybe.withDefault "" placeholder)
        }
