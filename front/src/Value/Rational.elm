module Value.Rational exposing (Rational(..), add, decoder, encode, inv, mul, neg, pow)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Rational
    = Rational Int Int


neg : Rational -> Rational
neg (Rational n d) =
    Rational -n d


inv : Rational -> Rational
inv (Rational n d) =
    Rational d n


mul : Rational -> Rational -> Rational
mul (Rational n1 d1) (Rational n2 d2) =
    Rational (n1 * n2) (d1 * d2)


div : Rational -> Rational -> Rational
div (Rational n1 d1) (Rational n2 d2) =
    Rational (n1 * d2) (d1 * n2)


add : Rational -> Rational -> Rational
add (Rational n1 d1) (Rational n2 d2) =
    Rational ((n1 * d2) + (n2 * d1)) (d1 * d2)


sub : Rational -> Rational -> Rational
sub (Rational n1 d1) (Rational n2 d2) =
    Rational ((n1 * d2) - (n2 * d1)) (d1 * d2)


pow : Int -> Rational -> Rational
pow p (Rational n d) =
    Rational (n ^ p) (d ^ p)


gcd : Int -> Int -> Int
gcd x y =
    if x == 0 then
        y

    else
        gcd (remainderBy x y) x


lcm : Int -> Int -> Int
lcm x y =
    if x == 0 || y == 0 then
        0

    else
        abs (toFloat x / toFloat (gcd x y * y) |> truncate)


decoder : Decoder Rational
decoder =
    Decode.string
        |> Decode.andThen
            (String.split "/"
                >> (\l ->
                        List.head l
                            |> Maybe.andThen String.toInt
                            >> Maybe.map
                                (\n ->
                                    List.tail l
                                        |> Maybe.map (String.join "/")
                                        >> Maybe.andThen String.toInt
                                        >> Maybe.map
                                            (Rational n >> Decode.succeed)
                                        >> Maybe.withDefault (Decode.fail "The denominator is not an integer")
                                )
                            >> Maybe.withDefault (Decode.fail "The numerator is not an integer")
                   )
            )


encode : Rational -> Encode.Value
encode (Rational n d) =
    Encode.string (String.fromInt n ++ "/" ++ String.fromInt d)
