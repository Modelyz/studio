module Expression.Unary exposing (..)

import Expression.Rational as R exposing (Rational)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Operator
    = Neg
    | Inv


eval : Operator -> Rational -> Rational
eval op n =
    case op of
        Neg ->
            R.neg n

        Inv ->
            R.inv n


toString : Operator -> String
toString o =
    case o of
        Neg ->
            "Neg"

        Inv ->
            "Inv"


toShortString : Operator -> String
toShortString o =
    case o of
        Neg ->
            " - "

        Inv ->
            "1 / "


all : List Operator
all =
    [ Neg, Inv ]


encode : Operator -> Encode.Value
encode op =
    case op of
        Neg ->
            Encode.object [ ( "type", Encode.string "Neg" ) ]

        Inv ->
            Encode.object [ ( "type", Encode.string "Inv" ) ]


decoder : Decoder Operator
decoder =
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
