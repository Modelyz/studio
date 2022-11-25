module Flow exposing (Flow(..), decoder, encode)

import Expression as Expression exposing (Expression)
import Expression.Rational as Rational exposing (Rational)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)
import Scope.Scope exposing (Scope(..))
import Value.Value as Value exposing (Value)


type Flow
    = ResourceFlow Resource
    | ResourceTypeFlow ResourceType


encode : Flow -> Encode.Value
encode flow =
    case flow of
        ResourceFlow resource ->
            Encode.object [ ( "type", Encode.string "ResourceFlow" ), ( "resource", Resource.encode resource ) ]

        ResourceTypeFlow resourceType ->
            Encode.object [ ( "type", Encode.string "ResourceTypeFlow" ), ( "resourceType", ResourceType.encode resourceType ) ]


decoder : Decoder Flow
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "ResourceFlow" ->
                        Decode.map ResourceFlow
                            (Decode.field "resource" Resource.decoder)

                    "ResourceTypeFlow" ->
                        Decode.map ResourceTypeFlow
                            (Decode.field "resourceType" ResourceType.decoder)

                    _ ->
                        Decode.fail "Unknown Flow type"
            )
