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
    = AllEntities Type
      -- or all the Type entities of a certain user type:
    | AllEntitiesOfType Type Uuid


fromEntity : Entity -> Scope
fromEntity e =
    let
        t =
            toType e
    in
    e |> toTypeUuid |> Maybe.map (AllEntitiesOfType t) |> Maybe.withDefault (AllEntities (toType e))


getParent : DictSet String Entity -> Scope -> Maybe Scope
getParent allEntities scope =
    case scope of
        AllEntities _ ->
            Nothing

        AllEntitiesOfType _ uuid ->
            Entity.fromUuid allEntities uuid
                |> Maybe.andThen Entity.toTypeUuid
                |> Maybe.andThen (Entity.fromUuid allEntities)
                |> Maybe.map fromEntity
                -- TODO maybe include the Type in the AllEntitiesOfType to avoid looking it up
                |> (\parent ->
                        case parent of
                            Just p ->
                                Just p

                            Nothing ->
                                uuid |> Entity.fromUuid allEntities |> Maybe.map toType |> Maybe.map AllEntities
                   )


getParentsToRoot : Scope -> DictSet String Entity -> List Scope -> List Scope
getParentsToRoot scope allEntities currentList =
    getParent allEntities scope
        |> Maybe.map (\parent -> getParentsToRoot parent allEntities (scope :: currentList))
        |> Maybe.withDefault (scope :: currentList)


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
                        && (Maybe.map3 Entity.isParentOf (Entity.fromUuid allEntities parentTypeUuid) (Just allEntities) (Entity.fromUuid allEntities childTypeUuid)
                                |> Maybe.withDefault False
                           )



--within : Scope -> DictSet String Entity -> ( Type, Maybe Uuid ) -> Bool
--within scope entities ( t, mtu ) =
--    -- True if the entity (or its type) is within the scope
--    -- TODO remove?
--    case scope of
--        AllEntities type_ ->
--            t == type_
--
--        AllEntitiesOfType scopeTypeUuid ->
--            Maybe.withDefault False <|
--                Maybe.map
--                    (\u ->
--                        let
--                            maybeScopeType =
--                                fromUuid entities scopeTypeUuid
--
--                            maybeEntity =
--                                fromUuid entities u
--                        in
--                        maybeEntity
--                            |> Maybe.andThen (\entityTypeUuid -> Maybe.map (\scopeType -> isChildOf scopeType entities entityTypeUuid) maybeScopeType)
--                            |> Maybe.withDefault False
--                    )
--                    mtu


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
