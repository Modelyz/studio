module Flow exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)
import Value.Rational as Rational exposing (Rational(..))


type Flow
    = Inflow Rational Resource
    | TypeInflow Rational ResourceType
    | Outflow Rational Resource
    | TypeOutflow Rational ResourceType


encode : Flow -> Encode.Value
encode flow =
    case flow of
        Inflow qty resource ->
            Encode.object
                [ ( "type", Encode.string "Inflow" )
                , ( "qty", Rational.encode qty )
                , ( "resource", Resource.encode resource )
                ]

        TypeInflow qty resourceType ->
            Encode.object
                [ ( "type", Encode.string "TypeInflow" )
                , ( "qty", Rational.encode qty )
                , ( "resourceType", ResourceType.encode resourceType )
                ]

        Outflow qty resource ->
            Encode.object
                [ ( "type", Encode.string "*Outflow" )
                , ( "qty", Rational.encode qty )
                , ( "resource", Resource.encode resource )
                ]

        TypeOutflow qty resourceType ->
            Encode.object
                [ ( "type", Encode.string "TypeOutflow" )
                , ( "qty", Rational.encode qty )
                , ( "resourceType", ResourceType.encode resourceType )
                ]


decoder : Decoder Flow
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Inflow" ->
                        Decode.map2 Inflow
                            (Decode.field "qty" Rational.decoder)
                            (Decode.field "resource" Resource.decoder)

                    "TypeInflow" ->
                        Decode.map2 TypeInflow
                            (Decode.field "qty" Rational.decoder)
                            (Decode.field "resourceType" ResourceType.decoder)

                    "Outflow" ->
                        Decode.map2 Outflow
                            (Decode.field "qty" Rational.decoder)
                            (Decode.field "resource" Resource.decoder)

                    "TypeOutflow" ->
                        Decode.map2 TypeOutflow
                            (Decode.field "qty" Rational.decoder)
                            (Decode.field "resourceType" ResourceType.decoder)

                    _ ->
                        Decode.fail "Unknown Flow type"
            )
