module Value.Expression exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Input as Input
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid exposing (Uuid)
import Time exposing (Posix)
import Type exposing (Type)
import Value.Observable as Observable exposing (Observable)
import Value.Rational as R exposing (Rational(..))


type
    Expression a
    -- a is commonly an Obs
    = Leaf a
    | Unary UOperator (Expression a)
    | Binary BOperator (Expression a) (Expression a)


type UOperator
    = Neg
    | Inv


updateExpr : List Int -> List Int -> Expression Observable -> Expression Observable -> Expression Observable
updateExpr targetPath currentPath subExpr expr =
    -- we replace the expr at the given path
    case expr of
        Leaf obs ->
            if currentPath == targetPath then
                subExpr

            else
                expr

        Unary o e ->
            Unary o (updateExpr targetPath (1 :: currentPath) subExpr e)

        Binary o e1 e2 ->
            Binary o (updateExpr targetPath (2 :: currentPath) subExpr e1) (updateExpr targetPath (3 :: currentPath) subExpr e2)


uToString : UOperator -> String
uToString o =
    case o of
        Neg ->
            "Neg"

        Inv ->
            "Inv"


uToShortString : UOperator -> String
uToShortString o =
    case o of
        Neg ->
            " - "

        Inv ->
            "1 / "


bToString : BOperator -> String
bToString o =
    case o of
        Add ->
            "Add"

        Or _ ->
            "Or"


bToShortString : BOperator -> String
bToShortString o =
    case o of
        Add ->
            " + "

        Or _ ->
            " or "


allObs : List Observable
allObs =
    [ Observable.number "" "", Observable.Value Nothing ]


allUnary : List UOperator
allUnary =
    [ Neg, Inv ]


allBinary : List BOperator
allBinary =
    [ Add, or "" "" Nothing ]


type BOperator
    = Add
    | Or { name : String, desc : String, choice : Result String Bool }


or : String -> String -> Maybe Bool -> BOperator
or n d c =
    Or { name = n, desc = d, choice = Result.fromMaybe "No choice made" c }


number : Expression Observable
number =
    Leaf <| Observable.number "truc" "machin"


add : Expression a -> Expression a -> Expression a
add e f =
    Binary Add e f


neg : Expression a -> Expression a
neg e =
    Unary Neg e


eval : Expression Observable -> Result String Float
eval expr =
    case expr of
        Leaf obs ->
            Observable.eval obs

        Unary op e ->
            Result.map (uEval op) (eval e)

        Binary op e f ->
            -- the error is displayed only for the 1st eval even if both fail
            bEval op (eval e) (eval f)


map : (Observable -> Float) -> Expression Observable -> Expression Float
map f expr =
    case expr of
        Leaf o ->
            Leaf (f o)

        Unary op expr1 ->
            Unary op (map f expr1)

        Binary op expr1 expr2 ->
            Binary op (map f expr1) (map f expr2)


encode : Expression Observable -> Encode.Value
encode e =
    case e of
        Leaf o ->
            Encode.object
                [ ( "type", Encode.string "Leaf" )
                , ( "obs", Observable.encode o )
                ]

        Unary operator expr ->
            Encode.object
                [ ( "type", Encode.string "Unary" )
                , ( "op", uEncode operator )
                , ( "expr", encode expr )
                ]

        Binary operator expr1 expr2 ->
            Encode.object
                [ ( "type", Encode.string "Binary" )
                , ( "op", bEncode operator )
                , ( "expr1", encode expr1 )
                , ( "expr2", encode expr2 )
                ]


decoder : Decoder (Expression Observable)
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Leaf" ->
                        Decode.map Leaf (Decode.field "obs" Observable.decoder)

                    "Unary" ->
                        Decode.map2 Unary (Decode.field "op" uDecoder) (Decode.field "expr" decoder)

                    "Binary" ->
                        Decode.map3 Binary (Decode.field "op" bDecoder) (Decode.field "expr1" decoder) (Decode.field "expr2" decoder)

                    _ ->
                        Decode.fail "Unknown Expression"
            )


uEval : UOperator -> Float -> Float
uEval op n =
    case op of
        Neg ->
            -n

        Inv ->
            1 / n


bEval : BOperator -> Result String Float -> Result String Float -> Result String Float
bEval operator res1 res2 =
    case operator of
        Add ->
            Result.map2 (+) res1 res2

        Or data ->
            -- one can choose either even if one fails
            data.choice
                |> Result.andThen
                    (\c ->
                        if c then
                            res1

                        else
                            res2
                    )


uEncode : UOperator -> Encode.Value
uEncode op =
    case op of
        Neg ->
            Encode.object [ ( "type", Encode.string "Neg" ) ]

        Inv ->
            Encode.object [ ( "type", Encode.string "Inv" ) ]


uDecoder : Decoder UOperator
uDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Neg" ->
                        Decode.succeed Neg

                    "Inv" ->
                        Decode.succeed Inv

                    _ ->
                        Decode.fail "Unknown Unary Operator"
            )


bEncode : BOperator -> Encode.Value
bEncode op =
    case op of
        Add ->
            Encode.object [ ( "type", Encode.string "Add" ) ]

        Or data ->
            Encode.object
                [ ( "type", Encode.string "type" )
                , ( "name", Encode.string data.name )
                , ( "desc", Encode.string data.desc )
                , ( "choice", Result.map Encode.bool data.choice |> Result.withDefault Encode.null )
                ]


bDecoder : Decoder BOperator
bDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Add" ->
                        Decode.succeed Add

                    "Or" ->
                        Decode.map3 or (Decode.field "name" Decode.string) (Decode.field "desc" Decode.string) (Decode.field "choice" (Decode.nullable Decode.bool))

                    _ ->
                        Decode.fail "Unknown Unary Operator"
            )
