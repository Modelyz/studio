module Expression.Input exposing (Config, inputExpression)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Expression as Expression exposing (BOperator, Expression(..), UOperator)
import Expression.DeepLink as DeepLink exposing (DeepLink(..))
import Expression.DeepLink.Select
import Expression.Eval as Eval
import Expression.HardLink as HardLink exposing (HardLink(..))
import Expression.Observable as Obs exposing (Observable(..))
import Expression.Rational as Rational
import Expression.ValueSelection as ValueSelection exposing (ValueSelection(..))
import Html.Attributes as Attr
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Scope.View exposing (selectScope)
import Shared
import Type exposing (Type)
import Typed.Type as TType
import Util exposing (chooseIfSingleton)
import Value.Value as Value exposing (..)
import View exposing (..)
import View.Style exposing (..)


type alias Config msg =
    { onEnter : msg
    , onInput : Expression -> msg
    , context : ( Type, Uuid )
    }


inputExpression : Config msg -> Shared.Model -> ( List Int, Expression ) -> Expression -> Element msg
inputExpression c s ( currentPath, currentExpr ) expr =
    -- used to input values into an expression
    case currentExpr of
        Leaf obs ->
            inputObservable c s currentPath obs expr

        Unary o e ->
            row [] [ text (Expression.uToShortString o), inputExpression c s ( 1 :: currentPath, e ) expr ]

        Binary o e1 e2 ->
            row [] [ text "( ", inputExpression c s ( 2 :: currentPath, e1 ) expr, text <| Expression.bToShortString o, inputExpression c s ( 3 :: currentPath, e2 ) expr, text " )" ]


inputObservable : Config msg -> Shared.Model -> List Int -> Observable -> Expression -> Element msg
inputObservable c s targetPath obs expr =
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
                        c.onInput <| Expression.updateExpr targetPath [] (Leaf <| ObsNumber { n | input = str, val = Rational.fromString str |> Result.map Tuple.first }) expr
                , text =
                    n.input
                , placeholder =
                    Just <| Input.placeholder [] <| text n.name
                , label =
                    if String.length n.name == 0 then
                        Input.labelHidden n.name

                    else
                        Input.labelLeft [ padding 10, Background.color color.content.background ]
                            (text (n.name ++ " ="))
                }

        ObsValue (ValueSelection.SelectedValue _ for name) ->
            row [ height fill, htmlAttribute <| Attr.title name ]
                [ text <|
                    case
                        Value.getByUuid for s.state.values
                            |> Result.andThen (Eval.veval s { context = c.context } s.state.values)
                    of
                        Err err ->
                            err

                        Ok r ->
                            Rational.toString r
                ]

        ObsValue ValueSelection.UndefinedValue ->
            row [ height fill ] [ text "Unselected value" ]

        ObsLink deeplink ->
            let
                value =
                    (Eval.dleval s deeplink [ Tuple.second c.context ]
                        |> chooseIfSingleton
                    )
                        |> Maybe.map (Eval.veval s { context = c.context } s.state.values)
            in
            row [ height fill ]
                [ value
                    |> Maybe.map
                        (\result ->
                            case
                                result
                            of
                                Err err ->
                                    err

                                Ok r ->
                                    Rational.toString r
                        )
                    |> Maybe.withDefault "EMPTY RESULT"
                    |> text
                ]
