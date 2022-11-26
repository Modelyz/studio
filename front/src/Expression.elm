module Expression exposing (BOperator(..), Expression(..), UOperator(..), Value, allBinary, allUnary, applyB, applyU, bToShortString, bToString, decoder, encode, eval, uToShortString, uToString, undo, updateExpr)

import Dict exposing (Dict)
import Expression.DeepLink exposing (DeepLink(..))
import Expression.Observable as Obs exposing (Observable(..))
import Expression.Rational as R exposing (Rational)
import Expression.ValueSelection exposing (ValueSelection(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid exposing (Uuid)
import Type exposing (Type)


type Expression
    = Leaf Observable
    | Unary UOperator Expression
    | Binary BOperator Expression Expression


type BOperator
    = Add
    | Multiply
    | Or { name : String, desc : String, choice : Result String Bool }


type UOperator
    = Neg
    | Inv


type alias Value =
    -- WARNING must be kept in sync with Value.Value
    { what : Type, for : Uuid, name : String, expr : Expression }


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


applyU : UOperator -> List Expression -> List Expression
applyU o stack =
    Maybe.map2 (\h t -> Unary o h :: t) (List.head stack) (List.tail stack) |> Maybe.withDefault []


applyB : BOperator -> List Expression -> List Expression
applyB o stack =
    Maybe.map3 (\f s t -> Binary o f s :: t) (List.head stack) ((List.tail >> Maybe.andThen List.head) stack) ((List.tail >> Maybe.andThen List.tail) stack) |> Maybe.withDefault []


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

        Multiply ->
            "Multiply"

        Or _ ->
            "Or"


bToShortString : BOperator -> String
bToShortString o =
    case o of
        Add ->
            " + "

        Multiply ->
            " × "

        Or _ ->
            " or "


allUnary : List UOperator
allUnary =
    [ Neg, Inv ]


allBinary : List BOperator
allBinary =
    [ Add, Multiply, or "" "" Nothing ]


or : String -> String -> Maybe Bool -> BOperator
or n d c =
    Or { name = n, desc = d, choice = Result.fromMaybe "No choice made" c }


eval : Dict String Value -> Expression -> Result String Rational
eval allVals expr =
    case expr of
        Leaf obs ->
            case obs of
                ObsNumber n ->
                    n.val

                ObsValue vs ->
                    case vs of
                        UndefinedValue ->
                            Err "Undefined"

                        SelectedValue w f n ->
                            allVals
                                |> Dict.filter (\_ x -> x.what == w && x.for == f && x.name == n)
                                |> Dict.values
                                |> List.head
                                |> Result.fromMaybe "The value does not exist anymore"
                                |> Result.andThen (.expr >> eval allVals)

                ObsLink deeplink ->
                    case deeplink of
                        Null ->
                            Err "Unselected value"

                        EndPoint _ _ ->
                            -- FIXME
                            Err "Undefined"

                        Link _ _ ->
                            -- FIXME
                            Err "Undefined"

        Unary op e ->
            Result.map (uEval op) (eval allVals e)

        Binary op e f ->
            -- the error is displayed only for the 1st eval even if both fail
            bEval op (eval allVals e) (eval allVals f)


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


decoder : Decoder Expression
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Leaf" ->
                        Decode.map Leaf (Decode.field "obs" Obs.decoder)

                    "Unary" ->
                        Decode.map2 Unary (Decode.field "op" uDecoder) (Decode.field "expr" decoder)

                    "Binary" ->
                        Decode.map3 Binary (Decode.field "op" bDecoder) (Decode.field "expr1" decoder) (Decode.field "expr2" decoder)

                    _ ->
                        Decode.fail "Unknown Expression"
            )


uEval : UOperator -> Rational -> Rational
uEval op n =
    case op of
        Neg ->
            R.neg n

        Inv ->
            R.inv n


bEval : BOperator -> Result String Rational -> Result String Rational -> Result String Rational
bEval operator res1 res2 =
    case operator of
        Add ->
            Result.map2 R.add res1 res2

        Multiply ->
            Result.map2 R.multiply res1 res2

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
            Encode.object [ ( "type", Encode.string "+" ) ]

        Multiply ->
            Encode.object [ ( "type", Encode.string "*" ) ]

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
                    "+" ->
                        Decode.succeed Add

                    "*" ->
                        Decode.succeed Multiply

                    "Or" ->
                        Decode.map3 or (Decode.field "name" Decode.string) (Decode.field "desc" Decode.string) (Decode.field "choice" (Decode.nullable Decode.bool))

                    _ ->
                        Decode.fail "Unknown Unary Operator"
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
