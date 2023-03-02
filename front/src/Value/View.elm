module Value.View exposing (displayValueDict)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Expression.Eval as Eval
import Expression.Rational as Rational
import Shared
import Value.Value exposing (Value)
import View exposing (innerCell)
import View.Style exposing (..)


displayValueDict : Shared.Model -> Eval.Config -> String -> Dict String Value -> Dict String Value -> Element msg
displayValueDict s c default allV data =
    if Dict.size data > 0 then
        table [ width shrink, Background.color color.item.background ]
            { data = Dict.values data
            , columns =
                [ { header = none, width = fill, view = .name >> innerCell }
                , { header = none
                  , width = fill
                  , view =
                        .expr
                            >> Eval.exeval s.state c allV
                            >> Result.map Rational.toFloatString
                            >> Result.withDefault "(undefined)"
                            >> innerCell
                  }
                ]
            }

    else
        text default
