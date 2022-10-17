module Value.Observable exposing (Observable(..), ValueSelection(..), createValue, decoder, encode, eval, number, toString)

import Dict
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type
    Observable
    -- a single number with a name and a value
    = ObsNumber { name : String, desc : String, val : Result String Float }
      -- the value maybe existing for entity of gived type and uuid
    | ObsValue ValueSelection


type ValueSelection
    = SelectedValue { what : Type, for : Uuid, name : String }
    | UndefinedValue


createValue : Type -> Uuid -> String -> ValueSelection
createValue w f n =
    SelectedValue { what = w, for = f, name = n }


vToString : ValueSelection -> String
vToString v =
    case v of
        SelectedValue _ ->
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


eval : Observable -> Result String Float
eval obs =
    case obs of
        ObsNumber n ->
            n.val

        ObsValue vs ->
            case vs of
                UndefinedValue ->
                    Err "Undefined"

                SelectedValue v ->
                    {- allValues
                       |> Dict.filter (\x -> x.what == v.what && x.for == v.for && x.name == v.name)
                       |> Dict.values
                       |> List.head
                       |> Result.map
                       |> Result.fromMaybe "Value not found"
                    -}
                    Err "TODO"


number : String -> String -> Observable
number name desc =
    ObsNumber { name = name, desc = desc, val = Err (name ++ " is undefined") }


encode : Observable -> Encode.Value
encode obs =
    case obs of
        ObsNumber n ->
            Encode.object
                [ ( "type", Encode.string "Number" )
                , ( "name", Encode.string n.name )
                , ( "desc", Encode.string n.desc )
                , ( "val", Result.map Encode.float n.val |> Result.withDefault Encode.null )
                ]

        ObsValue v ->
            case v of
                SelectedValue value ->
                    Encode.object
                        [ ( "type", Encode.string <| vToString v )
                        , ( "what", Type.encode value.what )
                        , ( "for", Uuid.encode value.for )
                        , ( "name", Encode.string value.name )
                        ]

                UndefinedValue ->
                    Encode.object
                        [ ( "type", Encode.string <| vToString v )
                        , ( "what", Encode.null )
                        , ( "for", Encode.null )
                        , ( "name", Encode.null )
                        ]


decoder : Decoder Observable
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Number" ->
                        Decode.map3 (\n d v -> ObsNumber { name = n, desc = d, val = v })
                            (Decode.field "name" Decode.string)
                            (Decode.field "desc" Decode.string)
                            (Decode.field "val" (Decode.nullable Decode.float |> Decode.andThen (Result.fromMaybe "f" >> Decode.succeed)))

                    "SelectedValue" ->
                        Decode.map3 (\w f n -> ObsValue <| SelectedValue { what = w, for = f, name = n })
                            (Decode.field "what" Type.decoder)
                            (Decode.field "for" Uuid.decoder)
                            (Decode.field "name" Decode.string)

                    "UndefinedValue" ->
                        Decode.succeed (ObsValue UndefinedValue)

                    _ ->
                        Decode.fail "Unknown Observable type"
            )
