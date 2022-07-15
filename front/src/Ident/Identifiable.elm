module Ident.Identifiable exposing (..)

import DateTime exposing (..)
import DictSet as Set exposing (DictSet)
import Entity.Entity as Entity exposing (Entity(..), toUuid)
import EntityType.EntityType as EntityType exposing (toName)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)


type
    Identifiable
    -- What is identified
    = Entity Entity.Entity
    | EntityType EntityType.EntityType


compare : Identifiable -> String
compare i =
    case i of
        Entity e ->
            "Entity " ++ Entity.compare e

        EntityType et ->
            "EntityType " ++ EntityType.compare et


encode : Identifiable -> Encode.Value
encode i =
    case i of
        Entity e ->
            Entity.encode e

        EntityType et ->
            EntityType.encode et


decoder : Decoder Identifiable
decoder =
    -- some kind of intrusion into Entity and EntityType decoders
    Decode.field "what" Decode.string
        |> Decode.andThen
            (\w ->
                if (String.slice 0 4 <| String.reverse w) == "Type" then
                    Decode.map EntityType EntityType.decoder

                else
                    Decode.map Entity Entity.decoder
            )


toString : Identifiable -> String
toString identifiable =
    case identifiable of
        Entity e ->
            Entity.toString e

        EntityType et ->
            EntityType.toString et
