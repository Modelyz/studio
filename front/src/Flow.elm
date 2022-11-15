module Flow exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)
import Value.Rational as Rational exposing (Rational(..))


type Flow
    = RFlow Rational Resource
    | RTFlow Rational ResourceType


encode : Flow -> Encode.Value
encode flow =
    case flow of
        RFlow qty resource ->
            Encode.object
                [ ( "type", Encode.string "Inflow" )
                , ( "qty", Rational.encode qty )
                , ( "resource", Resource.encode resource )
                ]

        RTFlow qty resourceType ->
            Encode.object
                [ ( "type", Encode.string "TypeInflow" )
                , ( "qty", Rational.encode qty )
                , ( "resourceType", ResourceType.encode resourceType )
                ]


decoder : Decoder Flow
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "RFlow" ->
                        Decode.map2 RFlow
                            (Decode.field "qty" Rational.decoder)
                            (Decode.field "resource" Resource.decoder)

                    "RTFlow" ->
                        Decode.map2 RTFlow
                            (Decode.field "qty" Rational.decoder)
                            (Decode.field "resourceType" ResourceType.decoder)

                    _ ->
                        Decode.fail "Unknown Flow type"
            )
