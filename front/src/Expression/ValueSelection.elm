module Expression.ValueSelection exposing (ValueSelection(..), decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type ValueSelection
    = SelectedValue { what : Type, for : Uuid, name : String }
    | UndefinedValue


encode : ValueSelection -> Encode.Value
encode vs =
    case vs of
        SelectedValue sv ->
            Encode.object
                [ ( "type", Encode.string "SelectedValue" )
                , ( "what", Type.encode sv.what )
                , ( "for", Uuid.encode sv.for )
                , ( "name", Encode.string sv.name )
                ]

        UndefinedValue ->
            Encode.object
                [ ( "type", Encode.string "UndefinedValue" )
                , ( "what", Encode.null )
                , ( "for", Encode.null )
                , ( "name", Encode.null )
                ]


decoder : Decoder ValueSelection
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "SelectedValue" ->
                        Decode.map3 (\w f n -> SelectedValue { what = w, for = f, name = n })
                            (Decode.field "what" Type.decoder)
                            (Decode.field "for" Uuid.decoder)
                            (Decode.field "name" Decode.string)

                    "UndefinedValue" ->
                        Decode.succeed UndefinedValue

                    _ ->
                        Decode.fail "Unknown ValueSelection"
            )
