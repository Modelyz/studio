module Value.Input exposing (Config, Model, inputValues)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Scope.Scope as Scope exposing (Scope)
import Shared
import Value.DeepLink as DeepLink
import Value.Expression as Expression exposing (Expression(..))
import Value.Observable as Observable exposing (Observable(..))
import Value.Rational as Rational
import Value.Value as Value exposing (..)
import Value.ValueSelection as ValueSelection
import View exposing (..)
import View.Style exposing (..)


type alias Model a =
    { a | values : Dict String Value }


type alias Config msg =
    { onEnter : msg
    , onInput : Value -> msg
    }


inputValues : Config msg -> Shared.Model -> Model a -> Element msg
inputValues c s model =
    -- display the expression with input fields for each relevant valueType
    column [ spacing 10 ]
        (h2 "Input values"
            :: (model.values
                    |> Dict.values
                    |> List.map
                        (inputValue c s model)
                    |> withDefaultContent (p <| "Apparently there are no values defined for this entity. Please first create one.")
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
            Input.text
                [ width <| px <| Rational.adaptRF n.val
                , htmlAttribute <| Attr.title n.input
                , Background.color
                    (case n.val of
                        Ok r ->
                            color.content.background

                        Err "" ->
                            color.content.background

                        Err _ ->
                            color.item.warning
                    )
                ]
                { onChange =
                    \x ->
                        c.onInput { v | expr = Expression.updateExpr targetPath [] (Leaf <| ObsNumber { n | input = x, val = Rational.fromString x |> Result.map Tuple.first }) v.expr }
                , text =
                    n.input
                , placeholder =
                    Just <| Input.placeholder [] <| text n.name
                , label =
                    Input.labelRight [ padding 10, Background.color color.content.background ]
                        (text
                            (case n.val of
                                Ok r ->
                                    "= " ++ Rational.toString r

                                Err "" ->
                                    ""

                                Err _ ->
                                    "(invalid number)"
                            )
                        )
                }

        -- TODO
        ObsValue (ValueSelection.SelectedValue what for name) ->
            row [ height fill, htmlAttribute <| Attr.title name ]
                [ text <|
                    case
                        Value.getByUuid for s.state.values
                            |> Result.andThen (Value.eval s.state.values)
                    of
                        Err err ->
                            err

                        Ok r ->
                            Rational.toString r
                ]

        ObsValue ValueSelection.UndefinedValue ->
            row [ height fill ] [ text "Unselected value" ]

        ObsLink deeplink ->
            row [ height fill ]
                [ text <|
                    case
                        {- DeepLink.eval s.state.values deeplink
                           |> Result.andThen (Value.eval s.state.values)
                        -}
                        Err "TODO"
                    of
                        Err err ->
                            err

                        Ok r ->
                            Rational.toString r
                ]
