module Expression exposing (Expression(..), applyBinary, applyUnary, decoder, encode, swap, undo, updateExpr)

import Dict exposing (Dict)
import Expression.Binary as B
import Expression.DeepLink exposing (DeepLink(..))
import Expression.Observable as Obs exposing (Observable(..))
import Expression.Rational as R exposing (Rational)
import Expression.Unary as U
import Expression.ValueSelection exposing (ValueSelection(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid exposing (Uuid)
import Type exposing (Type)


type Expression
    = Leaf Observable
    | Unary U.Operator Expression
    | Binary B.Operator Expression Expression


applyBinary : B.Operator -> List Expression -> List Expression
applyBinary o stack =
    Maybe.map3 (\f s t -> Binary o f s :: t) (List.head stack) ((List.tail >> Maybe.andThen List.head) stack) ((List.tail >> Maybe.andThen List.tail) stack) |> Maybe.withDefault []


applyUnary : U.Operator -> List Expression -> List Expression
applyUnary o stack =
    Maybe.map2 (\h t -> Unary o h :: t) (List.head stack) (List.tail stack) |> Maybe.withDefault []


updateExpr : List Int -> List Int -> Expression -> Expression -> Expression
updateExpr targetPath currentPath subExpr expr =
    -- we replace the expr at the given path
    case expr of
        Leaf _ ->
            if currentPath == targetPath then
                subExpr

            else
                expr

        Unary o e ->
            Unary o (updateExpr targetPath (1 :: currentPath) subExpr e)

        Binary o e1 e2 ->
            Binary o (updateExpr targetPath (2 :: currentPath) subExpr e1) (updateExpr targetPath (3 :: currentPath) subExpr e2)


encode : Expression -> Encode.Value
encode e =
    case e of
        Leaf o ->
            Encode.object
                [ ( "type", Encode.string "Leaf" )
                , ( "obs", Obs.encode o )
                ]

        Unary operator expr ->
            Encode.object
                [ ( "type", Encode.string "Unary" )
                , ( "op", U.encode operator )
                , ( "expr", encode expr )
                ]

        Binary operator expr1 expr2 ->
            Encode.object
                [ ( "type", Encode.string "Binary" )
                , ( "op", B.encode operator )
                , ( "expr1", encode expr1 )
                , ( "expr2", encode expr2 )
                ]


decoder : Decoder Expression
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Leaf" ->
                        Decode.map Leaf (Decode.field "obs" Obs.decoder)

                    "Unary" ->
                        Decode.map2 Unary (Decode.field "op" U.decoder) (Decode.field "expr" decoder)

                    "Binary" ->
                        Decode.map3 Binary (Decode.field "op" B.decoder) (Decode.field "expr1" decoder) (Decode.field "expr2" decoder)

                    _ ->
                        Decode.fail "Unknown Expression"
            )


undo : List Expression -> List Expression
undo stack =
    (List.head stack
        |> Maybe.map
            (\expr ->
                case expr of
                    Leaf _ ->
                        []

                    Unary _ e ->
                        [ e ]

                    Binary _ e1 e2 ->
                        [ e1, e2 ]
            )
        |> Maybe.withDefault []
    )
        ++ (List.tail stack |> Maybe.withDefault [])


swap : List Expression -> List Expression
swap stack =
    case stack of
        x :: xs ->
            case xs of
                y :: ys ->
                    y :: x :: ys

                _ ->
                    xs

        _ ->
            stack
