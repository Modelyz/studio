module Expression exposing (Expression(..), applyBinary, applyUnary, decoder, encode, swap, undo, updateExpr)

import Expression.Binary as B
import Expression.DeepLink exposing (DeepLink(..))
import Expression.Observable as Obs exposing (Observable(..))
import Expression.Unary as U
import Expression.ValueSelection exposing (ValueSelection(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Expression
    = Leaf Observable
    | Unary { uop : U.Operator, expr : Expression }
    | Binary { bop : B.Operator, expr1 : Expression, expr2 : Expression }


applyBinary : B.Operator -> List Expression -> List Expression
applyBinary o stack =
    Maybe.map3 (\f s t -> Binary { bop = o, expr1 = f, expr2 = s } :: t) (List.head stack) ((List.tail >> Maybe.andThen List.head) stack) ((List.tail >> Maybe.andThen List.tail) stack) |> Maybe.withDefault []


applyUnary : U.Operator -> List Expression -> List Expression
applyUnary o stack =
    Maybe.map2 (\h t -> Unary { uop = o, expr = h } :: t) (List.head stack) (List.tail stack) |> Maybe.withDefault []


updateExpr : List Int -> List Int -> Expression -> Expression -> Expression
updateExpr targetPath currentPath subExpr expr =
    -- we replace the expr at the given path
    case expr of
        Leaf _ ->
            if currentPath == targetPath then
                subExpr

            else
                expr

        Unary unary ->
            Unary { uop = unary.uop, expr = updateExpr targetPath (1 :: currentPath) subExpr unary.expr }

        Binary binary ->
            Binary { bop = binary.bop, expr1 = updateExpr targetPath (2 :: currentPath) subExpr binary.expr1, expr2 = updateExpr targetPath (3 :: currentPath) subExpr binary.expr2 }


encode : Expression -> Encode.Value
encode e =
    case e of
        Leaf o ->
            Encode.object
                [ ( "type", Encode.string "Leaf" )
                , ( "value", Obs.encode o )
                ]

        Unary unary ->
            Encode.object
                [ ( "type", Encode.string "Unary" )
                , ( "uop", U.encode unary.uop )
                , ( "expr", encode unary.expr )
                ]

        Binary binary ->
            Encode.object
                [ ( "type", Encode.string "Binary" )
                , ( "bop", B.encode binary.bop )
                , ( "expr1", encode binary.expr1 )
                , ( "expr2", encode binary.expr2 )
                ]


decoder : Decoder Expression
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Leaf" ->
                        Decode.map Leaf (Decode.field "value" Obs.decoder)

                    "Unary" ->
                        Decode.map2 (\o e -> Unary { uop = o, expr = e }) (Decode.field "uop" U.decoder) (Decode.field "expr" decoder)

                    "Binary" ->
                        Decode.map3 (\o e1 e2 -> Binary { bop = o, expr1 = e1, expr2 = e2 }) (Decode.field "bop" B.decoder) (Decode.field "expr1" decoder) (Decode.field "expr2" decoder)

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

                    Unary unary ->
                        [ unary.expr ]

                    Binary binary ->
                        [ binary.expr1, binary.expr2 ]
            )
        |> Maybe.withDefault []
    )
        ++ (List.tail stack |> Maybe.withDefault [])


swap : List Expression -> List Expression
swap stack =
    -- swap the two first lines of the stack
    case stack of
        x :: xs ->
            case xs of
                y :: ys ->
                    y :: x :: ys

                _ ->
                    xs

        _ ->
            stack
