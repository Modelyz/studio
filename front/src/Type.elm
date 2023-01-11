module Type exposing (Type(..), compare, decoder, encode, fromType, hasCommonParent, isChildOf, isParentOf, isType, parents, toHierarchic, toPluralString, toString, toType, typeOf)

import Dict exposing (Dict)
import Hierarchy.Type as HType
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Set
import Typed.Type as TType
import Util exposing (third)


type
    Type
    -- TODO see if it's still useful
    = TType TType.Type
    | HType HType.Type


toString : Type -> String
toString t =
    case t of
        TType tt ->
            TType.toString tt

        HType ht ->
            HType.toString ht


toPluralString : Type -> String
toPluralString t =
    case t of
        TType tt ->
            TType.toPluralString tt

        HType ht ->
            HType.toPluralString ht


fromString : String -> Maybe Type
fromString s =
    TType.fromString s
        |> Maybe.map TType
        |> (\t ->
                case t of
                    Just tt ->
                        Just tt

                    Nothing ->
                        HType.fromString s
                            |> Maybe.map HType
           )


encode : Type -> Encode.Value
encode =
    toString >> Encode.string


decoder : Decoder Type
decoder =
    Decode.string |> Decode.andThen (fromString >> Maybe.map Decode.succeed >> Maybe.withDefault (Decode.fail "Unknown Type"))


toHierarchic : Type -> Type
toHierarchic t =
    case t of
        TType tt ->
            HType (TType.toHierarchic tt)

        HType ht ->
            HType ht


compare : Type -> String
compare =
    toString


typeOf : Dict String ( Uuid, Type, Maybe Uuid ) -> Uuid -> Maybe Uuid
typeOf types uuid =
    Dict.get (Uuid.toString uuid) types |> Maybe.andThen third


isChildOf : Dict String ( Uuid, Type, Maybe Uuid ) -> Uuid -> Uuid -> Bool
isChildOf types parent uuid =
    if uuid == parent then
        True

    else
        Dict.get (Uuid.toString uuid) types
            |> Maybe.andThen third
            |> Maybe.map
                (\puuid ->
                    if parent == puuid then
                        True

                    else
                        isChildOf types parent puuid
                )
            |> Maybe.withDefault False


isParentOf : Dict String ( Uuid, Type, Maybe Uuid ) -> Uuid -> Uuid -> Bool
isParentOf types uuid parent =
    isChildOf types parent uuid


parents : Uuid -> Dict String ( Uuid, Type, Maybe Uuid ) -> List Uuid -> List Uuid
parents uuid items currents =
    -- TODO merge with Tree.parents
    Dict.get (Uuid.toString uuid) items
        |> Maybe.andThen third
        |> Maybe.map (\parent -> parents parent items (parent :: currents))
        |> Maybe.withDefault currents


hasCommonParent : Dict String ( Uuid, Type, Maybe Uuid ) -> Uuid -> Uuid -> Bool
hasCommonParent types uuid1 uuid2 =
    not <|
        Dict.isEmpty <|
            Dict.intersect
                (Dict.fromList <| List.map (\uuid -> ( Uuid.toString uuid, uuid )) <| parents uuid1 types [])
                (Dict.fromList <| List.map (\uuid -> ( Uuid.toString uuid, uuid )) <| parents uuid2 types [])


isType : Type -> Bool
isType t =
    (String.slice 0 4 <| String.reverse <| toString t) == "epyT"


toType : Type -> Type
toType t =
    case t of
        TType TType.Resource ->
            HType HType.ResourceType

        TType TType.Event ->
            HType HType.EventType

        TType TType.Agent ->
            HType HType.AgentType

        TType TType.Commitment ->
            HType HType.CommitmentType

        TType TType.Contract ->
            HType HType.ContractType

        TType TType.Process ->
            HType HType.ProcessType

        TType TType.Group ->
            HType HType.GroupType

        _ ->
            t


fromType : Type -> Type
fromType t =
    case t of
        HType HType.ResourceType ->
            TType TType.Resource

        HType HType.EventType ->
            TType TType.Event

        HType HType.AgentType ->
            TType TType.Agent

        HType HType.CommitmentType ->
            TType TType.Commitment

        HType HType.ContractType ->
            TType TType.Contract

        HType HType.ProcessType ->
            TType TType.Process

        HType HType.GroupType ->
            TType TType.Group

        _ ->
            t
