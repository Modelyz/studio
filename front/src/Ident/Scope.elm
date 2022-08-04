module Ident.Scope exposing (..)

import DictSet as Set exposing (DictSet)
import Element exposing (..)
import Entity.Entity as Entity exposing (Entity(..), fromUuid, isChildOf, isParentOf, toType, toTypeUuid, toUuid, toUuidString)
import Entity.Type as Type exposing (Type)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type
    Scope
    -- This is the scope of an identifier
    -- TODO: also identify all entities of a group ?
    -- or all the entities of a certain REA type:
    -- Scope could also be used in a group definition? TODO Move in its own module?
    = AllEntities Type
      -- or all the Type entities of a certain user type:
    | AllEntitiesOfType Type Uuid


fromEntity : Entity -> Maybe Scope
fromEntity e =
    let
        t =
            toType e
    in
    e |> toTypeUuid |> Maybe.map (AllEntitiesOfType t)


getParent : DictSet String Entity -> Scope -> Maybe Scope
getParent allEntities scope =
    -- get the parent scope but without the root typed scope
    case scope of
        AllEntities _ ->
            Nothing

        AllEntitiesOfType t uuid ->
            Entity.fromUuid allEntities uuid
                |> Maybe.andThen Entity.toTypeUuid
                |> Maybe.andThen (Entity.fromUuid allEntities)
                |> Maybe.andThen fromEntity


getParentsToRoot : Entity -> Scope -> DictSet String Entity -> List Scope -> List Scope
getParentsToRoot initialEntity scope allEntities currentList =
    getParent allEntities scope
        |> Maybe.map (\parentScope -> getParentsToRoot initialEntity parentScope allEntities currentList)
        |> Maybe.withDefault (AllEntities (toType initialEntity) :: scope :: currentList)


isParentOf : Scope -> DictSet String Entity -> Scope -> Bool
isParentOf childScope allEntities parentScope =
    case parentScope of
        AllEntities parentType ->
            case childScope of
                AllEntities childType ->
                    parentType == childType

                AllEntitiesOfType childType _ ->
                    parentType == childType

        AllEntitiesOfType parentType parentTypeUuid ->
            case childScope of
                AllEntities childType ->
                    False

                AllEntitiesOfType childType childTypeUuid ->
                    (parentType == childType)
                        && (Maybe.map3 Entity.isParentOf (Entity.fromUuid allEntities childTypeUuid) (Just allEntities) (Entity.fromUuid allEntities parentTypeUuid)
                                |> Maybe.withDefault False
                           )


toString : Scope -> String
toString id =
    case id of
        AllEntities _ ->
            "AllEntities"

        AllEntitiesOfType _ _ ->
            "AllEntitiesOfType"


encode : Scope -> Encode.Value
encode e =
    case e of
        AllEntities type_ ->
            Encode.object
                [ ( "for", Encode.string "AllEntities" )
                , ( "type", Type.encode type_ )
                ]

        AllEntitiesOfType type_ uuid ->
            Encode.object
                [ ( "for", Encode.string "AllEntitiesOfType" )
                , ( "type", Type.encode type_ )
                , ( "uuid", Uuid.encode uuid )
                ]


decoder : Decoder Scope
decoder =
    Decode.field "for" Decode.string
        |> Decode.andThen
            (\for ->
                case for of
                    "AllEntities" ->
                        Decode.map AllEntities
                            (Decode.field "type" Type.decoder)

                    "AllEntitiesOfType" ->
                        Decode.map2 AllEntitiesOfType
                            (Decode.field "type" Type.decoder)
                            (Decode.field "uuid" Uuid.decoder)

                    _ ->
                        Decode.fail "Cannot decode the scope of this identifier type"
            )


compare : Scope -> String
compare s =
    toString s
        ++ " "
        ++ (case s of
                AllEntities type_ ->
                    Type.toString type_

                AllEntitiesOfType type_ uuid ->
                    Type.toString type_ ++ " " ++ Uuid.toString uuid
           )
