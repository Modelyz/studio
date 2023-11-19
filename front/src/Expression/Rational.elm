module Expression.Rational exposing (Rational, add, decoder, denominator, divide, encode, fromString, inv, multiply, neg, numerator, one, parse, resultToString, toFloatString, toString, zero)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Rational
    = Rational Int Int


numerator : Rational -> Int
numerator (Rational n _) =
    n


denominator : Rational -> Int
denominator (Rational _ d) =
    d


zero : Rational
zero =
    Rational 0 1


one : Rational
one =
    Rational 1 1


neg : Rational -> Rational
neg (Rational n d) =
    Rational -n d


inv : Rational -> Rational
inv (Rational n d) =
    Rational d n


add : Rational -> Rational -> Rational
add (Rational n1 d1) (Rational n2 d2) =
    Rational ((n1 * d2) + (n2 * d1)) (d1 * d2)


multiply : Rational -> Rational -> Rational
multiply (Rational n1 d1) (Rational n2 d2) =
    Rational (n1 * n2) (d1 * d2)


divide : Rational -> Rational -> Rational
divide r1 r2 =
    multiply r1 (inv r2)


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
                                                if right == 0 then
                                                    Err "Infinite number"

                                                else
                                                    Ok <| Rational left right

                                            Nothing ->
                                                Err "Invalid denominator"
                                   )
                        )
                    >> Maybe.withDefault (Err "Invalid numerator")
           )


fromFloatString : String -> String -> Result String Rational
fromFloatString sep =
    -- Parse a Float String to a Rational (with custom separator)
    String.split sep
        >> (\parts ->
                if List.length parts > 2 then
                    Nothing

                else
                    List.head parts
                        |> Maybe.andThen
                            (\left ->
                                List.tail parts
                                    |> Maybe.andThen
                                        (\tails ->
                                            let
                                                right =
                                                    String.concat tails

                                                lr =
                                                    String.length right
                                            in
                                            Maybe.map2 Rational (String.toInt (left ++ right)) (Just (10 ^ lr))
                                        )
                            )
           )
        >> Result.fromMaybe "Invalid floating point number"


fromInt : String -> Result String Rational
fromInt s =
    String.toInt s |> Maybe.map (\n -> Rational n 1) |> Result.fromMaybe s


fromPercent : String -> Result String Rational
fromPercent s =
    let
        percent =
            String.right 1 s

        number =
            String.slice 0 -1 s
    in
    if percent == "%" then
        oneOf "Invalid left part of the percentage" [ fromInt, fromFloatString ".", fromFloatString "," ] number
            |> Result.map (multiply (Rational 1 100))

    else
        Err "Invalid percentage"


fromString : String -> Result String Rational
fromString s =
    oneOf "Invalid number. It should look like an integer, float, rational or percentage"
        [ fromInt
        , fromSlashString
        , fromFloatString "."
        , fromFloatString ","
        , fromPercent
        ]
        s


oneOf : String -> List (String -> Result String Rational) -> String -> Result String Rational
oneOf error ds s =
    -- return the first successful result using a list of rational decoders
    case ds of
        [] ->
            Err error

        first :: rest ->
            case first s of
                Ok rational ->
                    Ok rational

                Err _ ->
                    oneOf error rest s


toString : Rational -> String
toString (Rational n d) =
    String.fromInt n
        ++ (if d == 1 then
                ""

            else
                "/" ++ String.fromInt d
           )


toFloatString : Rational -> String
toFloatString (Rational n d) =
    String.fromFloat (toFloat n / toFloat d)


resultToString : Result String Rational -> String
resultToString result =
    case result of
        Ok rational ->
            toString rational

        Err err ->
            err


parse : String -> String
parse =
    fromString >> resultToString


decoder : Decoder Rational
decoder =
    Decode.map2 Rational
        (Decode.field "numerator" Decode.int)
        (Decode.field "denominator" Decode.int)


encode : Rational -> Encode.Value
encode (Rational n d) =
    Encode.object
        [ ( "numerator", Encode.int n )
        , ( "denominator", Encode.int d )
        ]
