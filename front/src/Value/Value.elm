module Value.Value exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (Posix)
import Type exposing (Type)
import Value.Rational as R exposing (Rational(..))


type alias Value =
    { what : Type
    , for : Uuid
    , name : String
    , expr : Expression
    }


type Expression
    = Leaf Observable
    | Unary UOperator Expression
    | Binary BOperator Expression Expression


type BOperator
    = Add
    | Or { name : String, desc : String, choice : Result String Bool }


type UOperator
    = Neg
    | Inv


type
    Observable
    -- a single number with a name and a value
    = ObsNumber { name : String, input : String, val : Result String Rational }
      -- the value maybe existing for entity of gived type and uuid
    | ObsValue ValueSelection


type ValueSelection
    = SelectedValue Type Uuid String
    | UndefinedValue


compare : Value -> String
compare v =
    Type.compare v.what ++ "/" ++ Uuid.toString v.for ++ "/" ++ v.name


fromUuid : Uuid -> Dict String Value -> Dict String Value
fromUuid uuid =
    Dict.filter (\_ v -> uuid == v.for)


encode : Value -> Encode.Value
encode v =
    Encode.object
        [ ( "what", Type.encode v.what )
        , ( "for", Uuid.encode v.for )
        , ( "name", Encode.string v.name )
        , ( "expr", eEncode v.expr )
        ]


decoder : Decoder Value
decoder =
    Decode.map4 Value
        (Decode.field "what" Type.decoder)
        (Decode.field "for" Uuid.decoder)
        (Decode.field "name" Decode.string)
        (Decode.field "expr" eDecoder)


updateExpr : List Int -> List Int -> Expression -> Expression -> Expression
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
    [ number "" "", ObsValue UndefinedValue ]


allUnary : List UOperator
allUnary =
    [ Neg, Inv ]


allBinary : List BOperator
allBinary =
    [ Add, or "" "" Nothing ]


or : String -> String -> Maybe Bool -> BOperator
or n d c =
    Or { name = n, desc = d, choice = Result.fromMaybe "No choice made" c }


add : Expression -> Expression -> Expression
add e f =
    Binary Add e f


neg : Expression -> Expression
neg e =
    Unary Neg e


eval : Dict String Value -> Expression -> Result String Rational
eval allVals expr =
    case expr of
        Leaf obs ->
            oEval allVals obs

        Unary op e ->
            Result.map (uEval op) (eval allVals e)

        Binary op e f ->
            -- the error is displayed only for the 1st eval even if both fail
            bEval op (eval allVals e) (eval allVals f)


eEncode : Expression -> Encode.Value
eEncode e =
    case e of
        Leaf o ->
            Encode.object
                [ ( "type", Encode.string "Leaf" )
                , ( "obs", oEncode o )
                ]

        Unary operator expr ->
            Encode.object
                [ ( "type", Encode.string "Unary" )
                , ( "op", uEncode operator )
                , ( "expr", eEncode expr )
                ]

        Binary operator expr1 expr2 ->
            Encode.object
                [ ( "type", Encode.string "Binary" )
                , ( "op", bEncode operator )
                , ( "expr1", eEncode expr1 )
                , ( "expr2", eEncode expr2 )
                ]


eDecoder : Decoder Expression
eDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Leaf" ->
                        Decode.map Leaf (Decode.field "obs" oDecoder)

                    "Unary" ->
                        Decode.map2 Unary (Decode.field "op" uDecoder) (Decode.field "expr" eDecoder)

                    "Binary" ->
                        Decode.map3 Binary (Decode.field "op" bDecoder) (Decode.field "expr1" eDecoder) (Decode.field "expr2" eDecoder)

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


vToString : ValueSelection -> String
vToString v =
    case v of
        SelectedValue _ _ _ ->
            "SelectedValue"

        UndefinedValue ->
            "UndefinedValue"


toString : Observable -> String
toString obs =
    case obs of
        ObsNumber _ ->
            "Number"

        ObsValue _ ->
            "Value"


oEval : Dict String Value -> Observable -> Result String Rational
oEval allVals obs =
    -- TODO move to Rational (or is a rational an expression??)
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


number : String -> String -> Observable
number name input =
    ObsNumber { name = name, input = input, val = Err "" }


oEncode : Observable -> Encode.Value
oEncode obs =
    case obs of
        ObsNumber n ->
            Encode.object
                [ ( "type", Encode.string "Number" )
                , ( "name", Encode.string n.name )
                , ( "input", Encode.string n.input )
                , ( "val", Result.map R.encode n.val |> Result.withDefault (Encode.string "") )
                ]

        ObsValue v ->
            case v of
                SelectedValue w f n ->
                    Encode.object
                        [ ( "type", Encode.string <| vToString v )
                        , ( "what", Type.encode w )
                        , ( "for", Uuid.encode f )
                        , ( "name", Encode.string n )
                        ]

                UndefinedValue ->
                    Encode.object
                        [ ( "type", Encode.string <| vToString v )
                        , ( "what", Encode.null )
                        , ( "for", Encode.null )
                        , ( "name", Encode.null )
                        ]


oDecoder : Decoder Observable
oDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Number" ->
                        Decode.map3 (\n i v -> ObsNumber { name = n, input = i, val = v })
                            (Decode.field "name" Decode.string)
                            (Decode.field "input" Decode.string)
                            (Decode.field "val" R.decoder)

                    "SelectedValue" ->
                        Decode.map3 SelectedValue
                            (Decode.field "what" Type.decoder)
                            (Decode.field "for" Uuid.decoder)
                            (Decode.field "name" Decode.string)
                            |> Decode.andThen (\vs -> Decode.succeed (ObsValue vs))

                    "UndefinedValue" ->
                        Decode.succeed (ObsValue UndefinedValue)

                    _ ->
                        Decode.fail "Unknown Observable type"
            )
