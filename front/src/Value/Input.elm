module Value.Input exposing (Config, inputValues)

import Dict exposing (Dict)
import Element exposing (..)
import Expression exposing (Expression(..))
import Expression.Eval as Eval
import Expression.Input exposing (inputExpression)
import Expression.Observable exposing (Observable(..))
import Expression.Rational as Rational
import Prng.Uuid exposing (Uuid)
import State exposing (State)
import Type exposing (Type)
import Value.Value as Value exposing (..)
import View exposing (..)


type alias Config msg =
    { onEnter : msg
    , onInput : Value -> msg

    -- TODO check the Type is used
    , context : ( Type, Uuid ) -- the context is the current entity
    }


inputValues : Config msg -> State -> Dict String Value -> Element msg
inputValues c s values =
    -- display the expression with input fields for each relevant valueType
    column [ spacing 10 ]
        (h2 "Values"
            :: (values
                    |> Dict.values
                    |> List.map
                        (inputValue c s)
                    |> withDefaultContent (p <| "Apparently there are no values defined for this entity. Please first create one.")
                --TODO + link
               )
        )


inputValue : Config msg -> State -> Value -> Element msg
inputValue c s v =
    column []
        [ el [ paddingXY 0 10 ] <| text (v.name ++ " :")
        , row [ spacing 5 ]
            [ inputExpression { onEnter = c.onEnter, onInput = \expr -> c.onInput { v | expr = expr }, context = c.context } s ( [], v.expr ) v.expr

            -- display the evaluated expression:
            , case v.expr of
                Leaf (Variable _) ->
                    -- don't repeat the single number...
                    none

                _ ->
                    Eval.exeval s { context = c.context } s.values v.expr
                        |> (\r ->
                                case r of
                                    Ok val ->
                                        text <| "= " ++ Rational.toFloatString val

                                    Err err ->
                                        text <| "= error : " ++ err
                           )
            ]
        ]
