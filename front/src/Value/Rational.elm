module Value.Rational exposing (Rational(..), adaptRF, add, decoder, encode, fromFloatString, fromSlashString, fromString, inv, mul, multiply, neg, pow, toString)

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


multiply : Rational -> Rational -> Rational
multiply (Rational n1 d1) (Rational n2 d2) =
    Rational (n1 * n2) (d1 * d2)


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


fromSlashString : String -> Result String Rational
fromSlashString =
    -- Try to parse a "n/d" string
    -- the error string is the unconverted string
    String.split "/"
        >> (\split ->
                List.head split
                    |> Maybe.andThen String.toInt
                    >> Maybe.map
                        (\left ->
                            List.tail split
                                |> Maybe.map (String.join "/")
                                >> Maybe.andThen String.toInt
                                >> (\maybeRight ->
                                        case maybeRight of
                                            Just right ->
                                                Ok <| Rational left right

                                            Nothing ->
                                                Err (String.join "/" split)
                                   )
                        )
                    >> Maybe.withDefault (Err (String.join "/" split))
           )


sign : Int -> Int
sign n =
    if n >= 0 then
        1

    else
        -1


fromFloatString : String -> String -> Result String Rational
fromFloatString sep =
    -- Parse a Float String to a Rational (with custom separator)
    String.split sep
        >> (\split ->
                List.head split
                    |> Maybe.andThen
                        (\left ->
                            List.tail split
                                |> Maybe.andThen
                                    (\tails ->
                                        let
                                            right =
                                                String.join "" tails

                                            ll =
                                                String.length left

                                            lr =
                                                String.length right
                                        in
                                        Maybe.map2 Rational (String.toInt (left ++ right)) (Just ((*) 1 (10 ^ lr)))
                                    )
                        )
           )
        >> Result.fromMaybe "Invalid number"


fromInt : String -> Result String Rational
fromInt s =
    String.toInt s |> Maybe.map (\n -> Rational n 1) |> Result.fromMaybe s


fromString : String -> Result String ( Rational, String )
fromString s =
    oneOf [ fromInt, fromSlashString, fromFloatString ".", fromFloatString "," ] s |> Result.map (\r -> ( r, s ))


oneOf : List (String -> Result String Rational) -> String -> Result String Rational
oneOf ds s =
    -- return the first successful result using a list of rational decoders
    case ds of
        [] ->
            Err s

        first :: rest ->
            case first s of
                Ok rat ->
                    Ok rat

                Err err ->
                    oneOf rest s


toString : Rational -> String
toString (Rational n d) =
    String.fromInt n
        ++ (if d == 1 then
                ""

            else
                "/" ++ String.fromInt d
           )


decoder : Decoder (Result String Rational)
decoder =
    Decode.string
        |> Decode.map (fromString >> Result.map Tuple.first)


encode : Rational -> Encode.Value
encode (Rational n d) =
    Encode.string (String.fromInt n ++ "/" ++ String.fromInt d)


adaptRF : Result String Rational -> Int
adaptRF r =
    -- adapt the input form width to the content
    50
        + (case r of
            Ok (Rational n d) ->
                ((String.fromInt n |> String.length) + (String.fromInt d |> String.length)) |> (*) 10

            Err err ->
                9 * String.length err
          )
