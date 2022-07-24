module Ident.Scope exposing (..)

import DictSet as Set exposing (DictSet)
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
      -- or all the entities of a certain user type:
    | AllEntitiesOfType Uuid


fromEntity : Entity -> Scope
fromEntity e =
    Maybe.map AllEntitiesOfType (toTypeUuid e)
        |> Maybe.withDefault (AllEntities (toType e))


isParentOf : Scope -> DictSet String Entity -> Scope -> Bool
isParentOf childScope allEntities parentScope =
    case parentScope of
        AllEntities parentType ->
            case childScope of
                AllEntities childType ->
                    parentType == childType

                AllEntitiesOfType childTypeUuid ->
                    Maybe.map (Entity.toType >> (==) parentType) (Entity.fromUuid allEntities childTypeUuid) |> Maybe.withDefault False

        AllEntitiesOfType parentTypeUuid ->
            case childScope of
                AllEntities childType ->
                    False

                AllEntitiesOfType childTypeUuid ->
                    Maybe.map3 Entity.isParentOf (Entity.fromUuid allEntities parentTypeUuid) (Just allEntities) (Entity.fromUuid allEntities childTypeUuid) |> Maybe.withDefault False


within : Scope -> DictSet String Entity -> ( Type, Maybe Uuid ) -> Bool
within scope entities ( t, mtu ) =
    -- True if the entity (or its type) is within the scope
    -- TODO remove?
    case scope of
        AllEntities type_ ->
            t == type_

        AllEntitiesOfType scopeTypeUuid ->
            Maybe.withDefault False <|
                Maybe.map
                    (\u ->
                        let
                            maybeScopeType =
                                fromUuid entities scopeTypeUuid

                            maybeEntity =
                                fromUuid entities u
                        in
                        maybeEntity
                            |> Maybe.andThen (\entityTypeUuid -> Maybe.map (\scopeType -> isChildOf scopeType entities entityTypeUuid) maybeScopeType)
                            |> Maybe.withDefault False
                    )
                    mtu


toDesc : Scope -> String
toDesc id =
    case id of
        AllEntities type_ ->
            Type.toString type_

        AllEntitiesOfType uuid ->
            "entities of type " ++ Uuid.toString uuid


toString : Scope -> String
toString id =
    case id of
        AllEntities _ ->
            "AllEntities"

        AllEntitiesOfType _ ->
            "AllEntitiesOfType"


encode : Scope -> Encode.Value
encode e =
    case e of
        AllEntities type_ ->
            Encode.object
                [ ( "for", Encode.string "AllEntities" )
                , ( "type", Type.encode type_ )
                ]

        AllEntitiesOfType uuid ->
            Encode.object
                [ ( "for", Encode.string "AllEntitiesOfType" )
                , ( "uuid", Uuid.encode uuid )
                ]


decoder : Decoder Scope
decoder =
    Decode.field "for" Decode.string
        |> Decode.andThen
            (\for ->
                case for of
                    "AllEntities" ->
                        Decode.field "type" (Decode.map AllEntities Type.decoder)

                    "AllEntitiesOfType" ->
                        Decode.field "uuid" (Decode.map AllEntitiesOfType Uuid.decoder)

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

                AllEntitiesOfType uuid ->
                    Uuid.toString uuid
           )
