module Value.Input exposing (Config, inputValues)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Expression as Expression exposing (Expression(..))
import Expression.Input exposing (inputExpression)
import Expression.Observable exposing (Observable(..))
import Expression.Rational as Rational
import Html.Attributes as Attr
import Shared
import Value.Value as Value exposing (..)
import Value.ValueSelection as ValueSelection
import View exposing (..)
import View.Style exposing (..)


type alias Config msg =
    { onEnter : msg
    , onInput : Value -> msg
    }


inputValues : Config msg -> Shared.Model -> Dict String Value -> Element msg
inputValues c s values =
    -- display the expression with input fields for each relevant valueType
    column [ spacing 10 ]
        (h2 "Input values"
            :: (values
                    |> Dict.values
                    |> List.map
                        (inputValue c s)
                    |> withDefaultContent (p <| "Apparently there are no values defined for this entity. Please first create one.")
                --TODO + link
               )
        )


inputValue : Config msg -> Shared.Model -> Value -> Element msg
inputValue c s v =
    column []
        [ el [ paddingXY 0 10 ] <| text (v.name ++ " :")
        , row [ spacing 5 ]
            [ inputExpression { onEnter = c.onEnter, onInput = \expr -> c.onInput { v | expr = expr } } s ( [], v.expr ) v.expr

            -- display the evaluated expression:
            , case v.expr of
                Leaf (ObsNumber _) ->
                    -- don't repeat the single number...
                    none

                _ ->
                    Expression.eval s.state.values v.expr
                        |> (\r ->
                                case r of
                                    Ok val ->
                                        text <| "= " ++ Rational.toString val

                                    Err err ->
                                        text <| "= error : " ++ err
                           )
            ]
        ]
