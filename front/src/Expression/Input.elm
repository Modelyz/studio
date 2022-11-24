module Expression.Input exposing (Config, inputExpression)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Expression as Expression exposing (BOperator, Expression(..), UOperator)
import Expression.DeepLink as DeepLink exposing (DeepLink)
import Expression.DeepLink.Select
import Expression.Observable as Obs exposing (Observable(..))
import Html.Attributes as Attr
import Scope.Scope as Scope exposing (Scope(..))
import Scope.View exposing (selectScope)
import Shared
import Value.Rational as Rational
import Value.Select
import Value.Value as Value exposing (..)
import Value.ValueSelection as ValueSelection exposing (ValueSelection(..))
import View exposing (..)
import View.Style exposing (..)


type alias Config msg =
    { onEnter : msg
    , onInput : Value -> msg
    }


inputExpression : Config msg -> Shared.Model -> ( List Int, Expression ) -> Expression -> Value -> Element msg
inputExpression c s ( currentPath, expr ) ex v =
    -- used to input values into an expression
    case expr of
        Leaf obs ->
            inputObservable c s currentPath obs ex v

        Unary o e ->
            row [] [ text (Expression.uToShortString o), inputExpression c s ( 1 :: currentPath, e ) e v ]

        Binary o e1 e2 ->
            row [] [ text "( ", inputExpression c s ( 2 :: currentPath, e1 ) ex v, text <| Expression.bToShortString o, inputExpression c s ( 3 :: currentPath, e2 ) ex v, text " )" ]


inputObservable : Config msg -> Shared.Model -> List Int -> Observable -> Expression -> Value -> Element msg
inputObservable c s targetPath obs expr v =
    case obs of
        ObsNumber n ->
            Input.text
                [ width <| px <| Rational.adaptRF n.val
                , htmlAttribute <| Attr.title n.input
                , Background.color
                    (case n.val of
                        Ok _ ->
                            color.content.background

                        Err "" ->
                            color.content.background

                        Err _ ->
                            color.item.warning
                    )
                ]
                { onChange =
                    \str ->
                        c.onInput { v | expr = Expression.updateExpr targetPath [] (Leaf <| ObsNumber { n | input = str, val = Rational.fromString str |> Result.map Tuple.first }) expr }
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
        ObsValue (ValueSelection.SelectedValue _ for name) ->
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

        ObsLink _ ->
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
