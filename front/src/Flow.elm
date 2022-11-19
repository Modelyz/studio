module Flow exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)
import Value.Expression as Expression exposing (Expression)
import Value.Value as Value exposing (Value)


type Flow
    = ResourceFlow Expression Resource
    | ResourceTypeFlow Expression ResourceType


encode : Flow -> Encode.Value
encode flow =
    case flow of
        ResourceFlow qty resource ->
            Encode.object
                [ ( "type", Encode.string "Inflow" )
                , ( "qty", Expression.encode qty )
                , ( "resource", Resource.encode resource )
                ]

        ResourceTypeFlow qty resourceType ->
            Encode.object
                [ ( "type", Encode.string "TypeInflow" )
                , ( "qty", Expression.encode qty )
                , ( "resourceType", ResourceType.encode resourceType )
                ]


decoder : Decoder Flow
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "ResourceFlow" ->
                        Decode.map2 ResourceFlow
                            (Decode.field "qty" Expression.decoder)
                            (Decode.field "resource" Resource.decoder)

                    "ResourceTypeFlow" ->
                        Decode.map2 ResourceTypeFlow
                            (Decode.field "qty" Expression.decoder)
                            (Decode.field "resourceType" ResourceType.decoder)

                    _ ->
                        Decode.fail "Unknown Flow type"
            )
