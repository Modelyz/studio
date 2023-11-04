module Flow exposing (Flow(..), decoder, encode, flowOf, userTypeOf, uuidOf)

import Dict exposing (Dict)
import Hierarchy.Type as HType
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)
import Scope exposing (Scope(..))
import Type exposing (Type, typeOf)
import Typed.Type as TType


type
    Flow
    -- similar to Either Resource ResourceType
    = ResourceFlow Resource
    | ResourceTypeFlow ResourceType


flowOf : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String { a | flow : Flow } -> Dict String Resource -> Dict String ResourceType -> Uuid -> List Uuid
flowOf types entities resources resourceTypes uuid =
    case typeOf types uuid of
        Just (Type.TType TType.Resource) ->
            Dict.get (Uuid.toString uuid) entities
                |> Maybe.map
                    (\e ->
                        resources
                            |> Dict.filter (\_ v -> v.uuid == uuidOf e.flow)
                            |> Dict.values
                            |> List.map .uuid
                    )
                |> Maybe.withDefault []

        Just (Type.HType HType.ResourceType) ->
            Dict.get (Uuid.toString uuid) entities
                |> Maybe.map
                    (\e ->
                        resourceTypes
                            |> Dict.filter (\_ v -> v.uuid == uuidOf e.flow)
                            |> Dict.values
                            |> List.map .uuid
                    )
                |> Maybe.withDefault []

        _ ->
            []


encode : Flow -> Encode.Value
encode flow =
    case flow of
        ResourceFlow resource ->
            Encode.object [ ( "type", Encode.string "ResourceFlow" ), ( "value", Resource.encode resource ) ]

        ResourceTypeFlow resourceType ->
            Encode.object [ ( "type", Encode.string "ResourceTypeFlow" ), ( "value", ResourceType.encode resourceType ) ]


uuidOf : Flow -> Uuid
uuidOf flow =
    case flow of
        ResourceFlow r ->
            r.uuid

        ResourceTypeFlow rt ->
            rt.uuid


userTypeOf : Flow -> Type
userTypeOf flow =
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
                            (Decode.field "value" Resource.decoder)

                    "ResourceTypeFlow" ->
                        Decode.map ResourceTypeFlow
                            (Decode.field "value" ResourceType.decoder)

                    _ ->
                        Decode.fail "Unknown Flow type"
            )
