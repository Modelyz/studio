module Value.Observable exposing (Observable(..), decoder, encode, eval, number, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Observable
    = Number { name : String, desc : String, val : Result String Int }


toString : Observable -> String
toString obs =
    case obs of
        Number _ ->
            "Number"


eval : Observable -> Result String Int
eval obs =
    case obs of
        Number data ->
            data.val


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
                , ( "val", Result.map Encode.int n.val |> Result.withDefault Encode.null )
                ]


decoder : Decoder Observable
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Number" ->
                        Decode.map3 (\n d v -> Number { name = n, desc = d, val = v }) (Decode.field "name" Decode.string) (Decode.field "desc" Decode.string) (Decode.field "val" (Decode.nullable Decode.int |> Decode.andThen (Result.fromMaybe "f" >> Decode.succeed)))

                    _ ->
                        Decode.fail "Unknown Observable type"
            )
