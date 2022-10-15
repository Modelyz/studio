module Value.Input exposing (Config, Model, inputValues)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Scope.Scope as Scope exposing (Scope)
import Shared
import Value.Expression as Expression exposing (Expression(..), updateExpr)
import Value.Observable as Observable exposing (Observable)
import Value.Value as Value exposing (Value)
import View exposing (..)
import View.Style exposing (..)


type alias Model a =
    { a | values : Dict String Value }


type alias Config msg =
    { onEnter : msg
    , onInput : Value -> msg
    }


inputValues : Config msg -> Shared.Model -> Model a -> Scope -> Element msg
inputValues c s model scope =
    -- display the expression with input fields for each relevant valueType
    column [ spacing 10 ]
        (h2 "Input values"
            :: (model.values
                    |> Dict.values
                    |> List.map
                        (inputValue c s model)
                    |> withDefaultContent (p <| "Apparently there are no values defined for " ++ Scope.toString scope ++ ". Please first create one.")
                --TODO + link
               )
        )


inputValue : Config msg -> Shared.Model -> Model a -> Value -> Element msg
inputValue c s model v =
    inputExpression c s model ( [], v.expr ) v


inputExpression : Config msg -> Shared.Model -> Model a -> ( List Int, Expression Observable ) -> Value -> Element msg
inputExpression c s model ( currentPath, expr ) v =
    -- used to input values into an expression
    case expr of
        Leaf obs ->
            inputObservable c s model currentPath obs v

        Unary o e ->
            row [] [ text (Expression.uToShortString o), inputExpression c s model ( 1 :: currentPath, e ) v ]

        Binary o e1 e2 ->
            row [] [ text "( ", inputExpression c s model ( 2 :: currentPath, e1 ) v, text <| Expression.bToShortString o, inputExpression c s model ( 3 :: currentPath, e2 ) v, text " )" ]


inputObservable : Config msg -> Shared.Model -> Model a -> List Int -> Observable -> Value -> Element msg
inputObservable c s model targetPath obs v =
    case obs of
        Observable.Number n ->
            row []
                [ column [ Background.color color.item.background ]
                    [ row [ Font.size size.text.small ]
                        [ Input.text [ width (px 70) ]
                            { onChange =
                                \x ->
                                    c.onInput { v | expr = updateExpr targetPath [] (Leaf <| Observable.Number { n | val = String.toInt x |> Result.fromMaybe "invalid number" }) v.expr }
                            , text = Result.map String.fromInt n.val |> Result.withDefault ""
                            , placeholder =
                                Just <| Input.placeholder [] <| text "Value"
                            , label = Input.labelHidden <| "Value"
                            }
                        ]
                    ]
                ]

        Observable.Value mu ->
            row [ Background.color color.item.background, Font.size size.text.small, height fill ]
                [ text "TODO display value"
                ]
