module Expression.View exposing (Config, viewExpression)

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
import Expression.Rational as Rational
import Html.Attributes as Attr
import Scope.Scope as Scope exposing (Scope(..))
import Scope.View exposing (selectScope)
import Shared
import Value.Value as Value exposing (..)
import Value.ValueSelection as ValueSelection exposing (ValueSelection(..))
import View exposing (..)
import View.Style exposing (..)


type alias Config msg =
    { onEnter : msg
    , onInput : Expression -> msg
    }


viewExpression : Shared.Model -> Expression -> Element msg
viewExpression s expr =
    -- used to input values into an expression
    case expr of
        Leaf obs ->
            viewObservable s obs

        Unary o e ->
            row [] [ text (Expression.uToShortString o), viewExpression s e ]

        Binary o e1 e2 ->
            row [] [ text "( ", viewExpression s e1, text <| Expression.bToShortString o, viewExpression s e2, text " )" ]


viewObservable : Shared.Model -> Observable -> Element msg
viewObservable s obs =
    case obs of
        ObsNumber n ->
            text <| Rational.toRString n.val

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
