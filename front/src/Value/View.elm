module Value.View exposing (displayValueDict)

-- TODO file seems unused

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Expression as Expression
import Expression.Rational as Rational
import Value.Value exposing (Value)
import View exposing (headerCell, innerCell)
import View.Style exposing (..)


displayValueDict : String -> Dict String Value -> Dict String Value -> Element msg
displayValueDict default allV data =
    if Dict.size data > 0 then
        table [ width shrink, Background.color color.item.background ]
            { data = Dict.values data
            , columns =
                [ { header = none, width = fill, view = .name >> innerCell }
                , { header = none, width = fill, view = .expr >> Expression.eval allV >> Result.map Rational.toFloatString >> Result.withDefault "(undefined)" >> innerCell }
                ]
            }

    else
        text default
