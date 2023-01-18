module Expression.View exposing (Config, inputExpression, viewExpression)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Expression as Expression exposing (Expression(..))
import Expression.Binary as B
import Expression.DeepLink as DeepLink exposing (DeepLink(..))
import Expression.DeepLink.Select
import Expression.DeepLink.View
import Expression.Eval as Eval
import Expression.HardLink as HardLink exposing (HardLink(..))
import Expression.Observable as Obs exposing (Observable(..))
import Expression.Rational as Rational
import Expression.Unary as U
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


type alias Config =
    { context : ( Type, Uuid )
    }


viewExpression : Shared.Model -> Expression -> Element msg
viewExpression s expr =
    -- used to input values into an expression
    case expr of
        Leaf obs ->
            viewObservable s obs

        Unary o e ->
            row [] [ text (U.toShortString o), viewExpression s e ]

        Binary o e1 e2 ->
            wrappedRow [ width fill ] [ viewExpression s e1, text <| B.toShortString o, viewExpression s e2 ]


inputExpression : Shared.Model -> Config -> Expression -> Element msg
inputExpression s c expr =
    -- used to input values into an expression
    case expr of
        Leaf obs ->
            inputObservable s c obs

        Unary o e ->
            row [] [ text (U.toShortString o), inputExpression s c e ]

        Binary o e1 e2 ->
            wrappedRow [] [ inputExpression s c e1, text <| B.toShortString o, inputExpression s c e2 ]


viewObservable : Shared.Model -> Observable -> Element msg
viewObservable s obs =
    case obs of
        ObsNumber n ->
            text <|
                case n.name of
                    "" ->
                        "[" ++ Rational.toRString n.val ++ "]"

                    _ ->
                        "[" ++ n.name
                            ++ (if n.input /= "" then
                                    "=" ++ Rational.toRString n.val

                                else
                                    ""
                               )
                            ++ "]"

        ObsValue (ValueSelection.SelectedValue _ for name) ->
            row [ height fill, htmlAttribute <| Attr.title name ]
                [ Value.getByUuid for s.state.values
                    |> Result.map .name
                    |> Result.withDefault "(value not found)"
                    |> text
                ]

        ObsValue ValueSelection.UndefinedValue ->
            row [ height fill ] [ text "Unselected value" ]

        ObsLink deeplink ->
            row [ height fill ]
                [ text <| Expression.DeepLink.View.toDisplay s deeplink
                ]


inputObservable : Shared.Model -> Config -> Observable -> Element msg
inputObservable s c obs =
    case obs of
        ObsNumber n ->
            text <| Rational.toRString n.val

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
            row [ height fill ]
                [ text <|
                    case
                        Err "TODO1"
                    of
                        Err err ->
                            err

                        Ok r ->
                            Rational.toString r
                ]
