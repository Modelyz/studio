module Value.Observable exposing (Observable(..), decoder, encode, eval, number, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type
    Observable
    -- a single number with a name and a value
    = Number { name : String, desc : String, val : Result String Float }
      -- the value maybe existing for entity of gived type and uuid
    | Value (Maybe Uuid)


toString : Observable -> String
toString obs =
    case obs of
        Number _ ->
            "Number"

        Value _ ->
            "Value"


eval : Observable -> Result String Float
eval obs =
    case obs of
        Number data ->
            data.val

        Value data ->
            -- TODO lookup the value and evaluate the expression
            Ok 0


number : String -> String -> Observable
number name desc =
    Number { name = name, desc = desc, val = Err (name ++ " is undefined") }


encode : Observable -> Encode.Value
encode obs =
    case obs of
        Number n ->
            Encode.object
                [ ( "type", Encode.string "Number" )
                , ( "name", Encode.string n.name )
                , ( "desc", Encode.string n.desc )
                , ( "val", Result.map Encode.float n.val |> Result.withDefault Encode.null )
                ]

        Value mu ->
            Encode.object
                [ ( "type", Encode.string "Value" )
                , ( "uuid", mu |> Maybe.map Uuid.encode |> Maybe.withDefault Encode.null )
                ]


decoder : Decoder Observable
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Number" ->
                        Decode.map3 (\n d v -> Number { name = n, desc = d, val = v })
                            (Decode.field "name" Decode.string)
                            (Decode.field "desc" Decode.string)
                            (Decode.field "val" (Decode.nullable Decode.float |> Decode.andThen (Result.fromMaybe "f" >> Decode.succeed)))

                    "Value" ->
                        Decode.map Value
                            (Decode.field "uuid" (Decode.nullable Uuid.decoder))

                    _ ->
                        Decode.fail "Unknown Observable type"
            )
