module Ident.Scope exposing (..)

import DateTime exposing (..)
import DictSet as Set exposing (DictSet)
import Entity.Entity as Entity exposing (Entity(..), toUuid)
import EntityType.EntityType as EntityType exposing (toName)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)


type
    Scope
    -- This is the scope of an identifier
    -- TODO: also identify all entities or of a group ?
    -- We can identify a particular entity:
    = OneEntity Entity.Entity
      -- or a certain entity type:
    | OneEntityType EntityType.EntityType
      -- or all the entities of a certain type:
    | AllEntityTypes EntityType.EntityType
      -- or all the entity types of a certain type:
    | AllEntities EntityType.EntityType


toDesc : Scope -> String
toDesc id =
    case id of
        OneEntity e ->
            "entity " ++ Uuid.toString (toUuid e)

        OneEntityType et ->
            toName et

        AllEntities et ->
            "entities of type " ++ toName et

        AllEntityTypes et ->
            "types whose parent type is " ++ toName et


toString : Scope -> String
toString id =
    case id of
        OneEntity e ->
            "OneEntity"

        OneEntityType et ->
            "OneEntityType"

        AllEntities et ->
            "AllEntities"

        AllEntityTypes et ->
            "AllEntityTypes"


encode : Scope -> Encode.Value
encode e =
    case e of
        OneEntity entity ->
            Encode.object
                [ ( "for", Encode.string "OneEntity" )
                , ( "entity", Entity.encode entity )
                ]

        OneEntityType entityType ->
            Encode.object
                [ ( "for", Encode.string "OneEntityType" )
                , ( "type", EntityType.encode entityType )
                ]

        AllEntities entityType ->
            Encode.object
                [ ( "for", Encode.string "AllEntities" )
                , ( "type", EntityType.encode entityType )
                ]

        AllEntityTypes entityType ->
            Encode.object
                [ ( "for", Encode.string "AllEntityTypes" )
                , ( "type", EntityType.encode entityType )
                ]


decoder : Decoder Scope
decoder =
    Decode.field "for" Decode.string
        |> Decode.andThen
            (\for ->
                case for of
                    "OneEntity" ->
                        Decode.field "type" (Decode.map OneEntity Entity.decoder)

                    "OneEntityType" ->
                        Decode.field "type" (Decode.map OneEntityType EntityType.decoder)

                    "AllEntities" ->
                        Decode.field "type" (Decode.map AllEntities EntityType.decoder)

                    "AllEntityTypes" ->
                        Decode.field "type" (Decode.map AllEntityTypes EntityType.decoder)

                    _ ->
                        Decode.fail "Cannot decode the scope of this identifier type"
            )


compare : Scope -> String
compare s =
    toString s
        ++ " "
        ++ (case s of
                OneEntity e ->
                    Entity.compare e

                OneEntityType et ->
                    EntityType.compare et

                AllEntities et ->
                    EntityType.compare et

                AllEntityTypes et ->
                    EntityType.compare et
           )
