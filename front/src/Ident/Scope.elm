module Ident.Scope exposing (..)

import DictSet as Set exposing (DictSet)
import Entity.Entity as Entity exposing (Entity(..), fromUuid, isChildOf, toType, toUuid, toUuidString)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type
    Scope
    -- This is the scope of an identifier
    -- TODO: also identify all entities or of a group ?
    -- We can identify a particular entity:
    = OneEntity Uuid
      -- or all the entities of a certain type:
    | AllEntitiesOfType Uuid


within : Scope -> DictSet String Entity -> Entity -> Bool
within scope entities entity =
    -- True if the entity is within the scope
    case scope of
        OneEntity uuid ->
            scope == OneEntity uuid

        AllEntitiesOfType uuid ->
            let
                parent =
                    fromUuid entities uuid
            in
            entity
                |> toType
                |> Maybe.andThen (fromUuid entities)
                |> Maybe.andThen (\e -> Maybe.map (\p -> isChildOf p entities e) parent)
                |> Maybe.withDefault False


toDesc : Scope -> String
toDesc id =
    case id of
        OneEntity uuid ->
            "entity " ++ Uuid.toString uuid

        AllEntitiesOfType uuid ->
            "entities of type " ++ Uuid.toString uuid


toString : Scope -> String
toString id =
    case id of
        OneEntity e ->
            "OneEntity"

        AllEntitiesOfType et ->
            "AllEntitiesOfType"


encode : Scope -> Encode.Value
encode e =
    case e of
        OneEntity uuid ->
            Encode.object
                [ ( "for", Encode.string "OneEntity" )
                , ( "uuid", Uuid.encode uuid )
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
                    "OneEntity" ->
                        Decode.field "uuid" (Decode.map OneEntity Uuid.decoder)

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
                OneEntity uuid ->
                    Uuid.toString uuid

                AllEntitiesOfType uuid ->
                    Uuid.toString uuid
           )
