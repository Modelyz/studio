module Ident.Scope exposing (..)

import DictSet as Set exposing (DictSet)
import Entity.Entity as Entity exposing (Entity(..), fromUuid, isChildOf, toTypeUuid, toUuid, toUuidString)
import Entity.Type as Type exposing (Type)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type
    Scope
    -- This is the scope of an identifier
    -- TODO: also identify all entities or of a group ?
    -- We can identify a particular entity:
    = OneEntity Uuid
      -- or all the entities of a certain REA type:
    | AllEntities Type
      -- or all the entities of a certain user type:
    | AllEntitiesOfType Uuid


within : Scope -> DictSet String Entity -> Maybe Entity -> Bool
within scope entities met =
    -- True if the entity is within the scope
    Maybe.withDefault True <|
        Maybe.map
            (\e ->
                case scope of
                    OneEntity uuid ->
                        scope == OneEntity uuid

                    AllEntities type_ ->
                        Maybe.withDefault False <| Maybe.map (\et -> type_ == Entity.toType et) met

                    AllEntitiesOfType uuid ->
                        let
                            parent =
                                fromUuid entities uuid
                        in
                        e
                            |> toTypeUuid
                            |> Maybe.andThen (fromUuid entities)
                            |> Maybe.andThen (\x -> Maybe.map (\p -> isChildOf p entities x) parent)
                            |> Maybe.withDefault False
            )
            met


toDesc : Scope -> String
toDesc id =
    case id of
        OneEntity uuid ->
            "entity " ++ Uuid.toString uuid

        AllEntities type_ ->
            Type.toString type_

        AllEntitiesOfType uuid ->
            "entities of type " ++ Uuid.toString uuid


toString : Scope -> String
toString id =
    case id of
        OneEntity _ ->
            "OneEntity"

        AllEntities _ ->
            "AllEntities"

        AllEntitiesOfType _ ->
            "AllEntitiesOfType"


encode : Scope -> Encode.Value
encode e =
    case e of
        OneEntity uuid ->
            Encode.object
                [ ( "for", Encode.string "OneEntity" )
                , ( "uuid", Uuid.encode uuid )
                ]

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
                    "OneEntity" ->
                        Decode.field "uuid" (Decode.map OneEntity Uuid.decoder)

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
                OneEntity uuid ->
                    Uuid.toString uuid

                AllEntities type_ ->
                    Type.toString type_

                AllEntitiesOfType uuid ->
                    Uuid.toString uuid
           )
