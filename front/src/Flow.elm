module Flow exposing (Flow(..), decoder, encode, flowOf, typeOf, uuidOf)

import Dict exposing (Dict)
import Expression as Expression exposing (Expression)
import Expression.Rational as Rational exposing (Rational)
import Hierarchy.Type as HType
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)
import Scope.Scope exposing (Scope(..))
import Type exposing (Type)
import Typed.Type as TType
import Value.Value as Value exposing (Value)


type Flow
    = ResourceFlow Resource
    | ResourceTypeFlow ResourceType


flowOf : Dict String { a | flow : Flow } -> Dict String Resource -> Dict String ResourceType -> Uuid -> List Uuid
flowOf entities resources resourceTypes uuid =
    Dict.get (Uuid.toString uuid) entities
        |> Maybe.map
            (\e ->
                resources
                    |> Dict.filter (\_ v -> v.uuid == uuidOf e.flow)
                    |> Dict.values
                    |> List.map .uuid
            )
        |> Maybe.withDefault []


encode : Flow -> Encode.Value
encode flow =
    case flow of
        ResourceFlow resource ->
            Encode.object [ ( "type", Encode.string "ResourceFlow" ), ( "resource", Resource.encode resource ) ]

        ResourceTypeFlow resourceType ->
            Encode.object [ ( "type", Encode.string "ResourceTypeFlow" ), ( "resourceType", ResourceType.encode resourceType ) ]


uuidOf : Flow -> Uuid
uuidOf flow =
    case flow of
        ResourceFlow r ->
            r.uuid

        ResourceTypeFlow rt ->
            rt.uuid


typeOf : Flow -> Type
typeOf flow =
    case flow of
        ResourceFlow _ ->
            Type.TType TType.Resource

        ResourceTypeFlow _ ->
            Type.HType HType.ResourceType


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
