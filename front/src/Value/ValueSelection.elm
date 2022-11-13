module Value.ValueSelection exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type ValueSelection
    = SelectedValue Type Uuid String
    | UndefinedValue


toString : ValueSelection -> String
toString v =
    case v of
        SelectedValue _ _ _ ->
            "SelectedValue"

        UndefinedValue ->
            "UndefinedValue"


encode : ValueSelection -> Encode.Value
encode vs =
    case vs of
        SelectedValue t u s ->
            Encode.object
                [ ( "type", Encode.string "SelectedValue" )
                , ( "what", Type.encode t )
                , ( "for", Uuid.encode u )
                , ( "name", Encode.string s )
                ]

        UndefinedValue ->
            Encode.object [ ( "type", Encode.string "UndefinedValue" ) ]


decoder : Decoder ValueSelection
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "SelectedValue" ->
                        Decode.map3 SelectedValue
                            (Decode.field "what" Type.decoder)
                            (Decode.field "for" Uuid.decoder)
                            (Decode.field "name" Decode.string)

                    "UndefinedValue" ->
                        Decode.succeed UndefinedValue

                    _ ->
                        Decode.fail "Unknown ValueSelection"
            )
