module Value.View exposing (displayValueDict)

-- TODO file seems unused

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Value.Expression as Expression
import Value.Value as Value exposing (Value)
import View exposing (headerCell, innerCell)
import View.Style exposing (..)


displayValueDict : String -> Dict String Value -> Element msg
displayValueDict default data =
    if Dict.size data > 0 then
        table [ width shrink, Background.color color.item.background ]
            { data = Dict.values data
            , columns =
                [ { header = headerCell color.table.header.background "Name", width = fill, view = .name >> innerCell }

                -- TODO : display the Err string
                , { header = headerCell color.table.header.background "Value", width = fill, view = .expr >> Expression.eval >> Result.map String.fromInt >> Result.withDefault "Error" >> innerCell }
                ]
            }

    else
        text default
