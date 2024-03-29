module Expression.View exposing (Config, inputExpression, viewExpression)

import Element exposing (..)
import Expression exposing (Expression(..))
import Expression.Binary as B
import Expression.DeepLink exposing (DeepLink(..))
import Expression.DeepLink.View
import Expression.Eval as Eval
import Expression.HardLink exposing (HardLink(..))
import Expression.Observable exposing (Observable(..))
import Expression.Rational as Rational
import Expression.Unary as U
import Expression.ValueSelection as ValueSelection exposing (ValueSelection(..))
import Html.Attributes as Attr
import Prng.Uuid exposing (Uuid)
import Scope exposing (Scope(..))
import State exposing (State)
import Type exposing (Type)
import Value.Value as Value exposing (..)
import View exposing (..)
import View.Style exposing (..)


type alias Config =
    { context : ( Type, Uuid )
    }


viewExpression : State -> Expression -> Element msg
viewExpression s expr =
    -- used to input values into an expression
    case expr of
        Leaf obs ->
            viewObservable s obs

        Unary unary ->
            wrappedRow [ width fill ] [ text (U.toShortString unary.uop), viewExpression s unary.expr ]

        Binary binary ->
            wrappedRow [ width fill ] [ viewExpression s binary.expr1, text <| B.toShortString binary.bop, viewExpression s binary.expr2 ]


inputExpression : State -> Config -> Expression -> Element msg
inputExpression s c expr =
    -- used to input values into an expression
    case expr of
        Leaf obs ->
            inputObservable s c obs

        Unary unary ->
            wrappedRow [ width fill ] [ text (U.toShortString unary.uop), inputExpression s c unary.expr ]

        Binary binary ->
            wrappedRow [ width fill ] [ inputExpression s c binary.expr1, text <| B.toShortString binary.bop, inputExpression s c binary.expr2 ]


viewObservable : State -> Observable -> Element msg
viewObservable s obs =
    case obs of
        Constant n ->
            text n

        Variable n ->
            text <|
                case n.name of
                    "" ->
                        "[" ++ Rational.parse n.input ++ "]"

                    _ ->
                        "["
                            ++ n.name
                            ++ (if n.input /= "" then
                                    "=" ++ Rational.parse n.input

                                else
                                    ""
                               )
                            ++ "]"

        ObsValue (ValueSelection.SelectedValue sv) ->
            wrappedRow [ height fill, htmlAttribute <| Attr.title sv.name ]
                [ Value.getByUuid sv.for s.values
                    |> Result.map .name
                    |> Result.withDefault "(value not found)"
                    |> text
                ]

        ObsValue ValueSelection.UndefinedValue ->
            wrappedRow [ height fill ] [ text "Unselected value" ]

        ObsLink deeplink ->
            wrappedRow [ height fill ]
                [ text <| Expression.DeepLink.View.toDisplay s deeplink
                ]


inputObservable : State -> Config -> Observable -> Element msg
inputObservable s c obs =
    case obs of
        Constant n ->
            text n

        Variable n ->
            text <| Rational.parse n.input

        ObsValue (ValueSelection.SelectedValue sv) ->
            wrappedRow [ width fill, height fill, htmlAttribute <| Attr.title sv.name ]
                [ text <|
                    case
                        Value.getByUuid sv.for s.values
                            |> Result.andThen (Eval.veval s { context = c.context } s.values)
                    of
                        Err err ->
                            err

                        Ok r ->
                            Rational.toString r
                ]

        ObsValue ValueSelection.UndefinedValue ->
            wrappedRow [ width fill, height fill ] [ text "Unselected value" ]

        ObsLink _ ->
            wrappedRow [ width fill, height fill ]
                [ text <|
                    case
                        Err "TODO1"
                    of
                        Err err ->
                            err

                        Ok r ->
                            Rational.toString r
                ]
