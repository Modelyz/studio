module Expression.Binary exposing (..)

import Expression.Rational as R exposing (Rational)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Operator
    = Add
    | Multiply
    | Or { name : String, desc : String, choice : Result String Bool }


toString : Operator -> String
toString o =
    case o of
        Add ->
            "Add"

        Multiply ->
            "Multiply"

        Or _ ->
            "Or"


eval : Operator -> Result String Rational -> Result String Rational -> Result String Rational
eval operator res1 res2 =
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


or : String -> String -> Maybe Bool -> Operator
or n d c =
    Or { name = n, desc = d, choice = Result.fromMaybe "No choice made" c }


toShortString : Operator -> String
toShortString o =
    case o of
        Add ->
            " + "

        Multiply ->
            " × "

        Or _ ->
            " or "


all : List Operator
all =
    [ Add, Multiply, or "" "" Nothing ]


encode : Operator -> Encode.Value
encode op =
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


decoder : Decoder Operator
decoder =
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