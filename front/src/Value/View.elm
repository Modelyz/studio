module Value.View exposing (displayValueDict)

-- TODO file seems unused

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Expression as Expression
import Expression.Eval as Eval
import Expression.Rational as Rational
import Prng.Uuid exposing (Uuid)
import Shared
import Type exposing (Type)
import Value.Value exposing (Value)
import View exposing (headerCell, innerCell)
import View.Style exposing (..)


displayValueDict : Shared.Model -> Eval.Config -> String -> Dict String Value -> Dict String Value -> Element msg
displayValueDict s c default allV data =
    if Dict.size data > 0 then
        table [ width shrink, Background.color color.item.background ]
            { data = Dict.values data
            , columns =
                [ { header = none, width = fill, view = .name >> innerCell }
                , { header = none, width = fill, view = .expr >> Eval.exeval s c allV >> Result.map Rational.toFloatString >> Result.withDefault "(undefined)" >> innerCell }
                ]
            }

    else
        text default
