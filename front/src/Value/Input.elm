module Value.Input exposing (Config, Model, inputValues)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Scope.Scope as Scope exposing (Scope)
import Shared
import Value.Expression as Expression exposing (Expression(..), updateExpr)
import Value.Observable as Observable exposing (Observable(..))
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
    column []
        [ el [ paddingXY 0 10 ] <| text (v.name ++ " :")
        , row [ spacing 5 ]
            [ inputExpression c s model ( [], v.expr ) v

            -- display the evaluated expression:
            , case v.expr of
                Leaf (ObsNumber _) ->
                    -- don't repeat the single number...
                    none

                _ ->
                    Expression.eval v.expr
                        |> (\r ->
                                case r of
                                    Ok val ->
                                        text <| "= " ++ String.fromFloat val

                                    Err err ->
                                        text <| "error: " ++ err
                           )
            ]
        ]


inputExpression : Config msg -> Shared.Model -> Model a -> ( List Int, Expression ) -> Value -> Element msg
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
        ObsNumber n ->
            row [ Background.color color.item.background, Font.size size.text.small ]
                [ Input.text [ width (px 70), htmlAttribute <| Attr.title n.desc ]
                    { onChange =
                        \x ->
                            c.onInput { v | expr = updateExpr targetPath [] (Leaf <| ObsNumber { n | val = String.toFloat x |> Result.fromMaybe "invalid number" }) v.expr }
                    , text = Result.map String.fromFloat n.val |> Result.withDefault ""
                    , placeholder =
                        Just <| Input.placeholder [] <| text n.name
                    , label = Input.labelHidden n.name
                    }
                ]

        ObsValue vs ->
            row [ Background.color color.item.background, Font.size size.text.small, height fill ]
                [ text <|
                    case Observable.eval (ObsValue vs) of
                        Err err ->
                            err

                        Ok r ->
                            String.fromFloat r
                ]
