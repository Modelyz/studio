module REA.Ident exposing (..)

import DateTime exposing (..)
import DictSet as Set exposing (DictSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid
import REA.Entity as EN exposing (Entity(..), toUuid)
import REA.EntityType as ENT exposing (toName)
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)


type alias Name =
    String


type
    Scope
    -- This is the scope of an identifier
    -- We can identify a particular entity:
    = OneEntity EN.Entity
      -- or a certain entity type:
    | OneEntityType ENT.EntityType
      -- or all the entities of a certain type:
    | AllEntityTypes ENT.EntityType
      -- or all the entity types of a certain type:
    | AllEntities ENT.EntityType



-- TODO: also identify all entities or of a group ?


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


encodeScope : Scope -> Encode.Value
encodeScope e =
    case e of
        OneEntity entity ->
            Encode.object
                [ ( "for", Encode.string "OneEntity" )
                , ( "entity", EN.encode entity )
                ]

        OneEntityType entityType ->
            Encode.object
                [ ( "for", Encode.string "OneEntityType" )
                , ( "type", ENT.encode entityType )
                ]

        AllEntities entityType ->
            Encode.object
                [ ( "for", Encode.string "AllEntities" )
                , ( "type", ENT.encode entityType )
                ]

        AllEntityTypes entityType ->
            Encode.object
                [ ( "for", Encode.string "AllEntityTypes" )
                , ( "type", ENT.encode entityType )
                ]


scopeDecoder : Decoder Scope
scopeDecoder =
    Decode.field "for" Decode.string
        |> Decode.andThen
            (\for ->
                case for of
                    "OneEntity" ->
                        Decode.field "type" (Decode.map OneEntity EN.decoder)

                    "OneEntityType" ->
                        Decode.field "type" (Decode.map OneEntityType ENT.decoder)

                    "AllEntities" ->
                        Decode.field "type" (Decode.map AllEntities ENT.decoder)

                    "AllEntityTypes" ->
                        Decode.field "type" (Decode.map AllEntityTypes ENT.decoder)

                    _ ->
                        Decode.fail "Cannot decode the scope of this identifier type"
            )


compare : Scope -> String
compare i =
    toString i
        ++ " "
        ++ (case i of
                OneEntity e ->
                    EN.compare e

                OneEntityType et ->
                    ENT.compare et

                AllEntities et ->
                    ENT.compare et

                AllEntityTypes et ->
                    ENT.compare et
           )
