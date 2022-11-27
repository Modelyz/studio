module Expression.View exposing (Config, viewExpression)

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


type alias Config =
    { context : ( Type, Uuid )
    }


viewExpression : Shared.Model -> Config -> Expression -> Element msg
viewExpression s c expr =
    -- used to input values into an expression
    case expr of
        Leaf obs ->
            viewObservable s c obs

        Unary o e ->
            row [] [ text (Expression.uToShortString o), viewExpression s c e ]

        Binary o e1 e2 ->
            row [] [ text "( ", viewExpression s c e1, text <| Expression.bToShortString o, viewExpression s c e2, text " )" ]


viewObservable : Shared.Model -> Config -> Observable -> Element msg
viewObservable s c obs =
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
                        Err "TODO"
                    of
                        Err err ->
                            err

                        Ok r ->
                            Rational.toString r
                ]
