module Value.View exposing (displayValueDict)

-- TODO file seems unused

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Value.Rational as Rational
import Value.Value as Value exposing (Value, eval)
import View exposing (headerCell, innerCell)
import View.Style exposing (..)


displayValueDict : String -> Dict String Value -> Dict String Value -> Element msg
displayValueDict default allV data =
    if Dict.size data > 0 then
        table [ width shrink, Background.color color.item.background ]
            { data = Dict.values data
            , columns =
                [ { header = headerCell color.table.header.background "Name", width = fill, view = .name >> innerCell }
                , { header = headerCell color.table.header.background "Value", width = fill, view = .expr >> eval allV >> Result.map Rational.toString >> Result.withDefault "" >> innerCell }
                ]
            }

    else
        text default
