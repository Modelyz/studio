module Ident.Identifiable exposing (..)

import DateTime exposing (..)
import DictSet as Set exposing (DictSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid
import REA.Entity as EN exposing (Entity(..), toUuid)
import REA.EntityType as ENT exposing (toName)
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)


type
    Identifiable
    -- What is identified
    = Entity EN.Entity
    | EntityType ENT.EntityType


isRelated : Entity -> Identifiable -> Bool
isRelated entity identifiable =
    case identifiable of
        EntityType _ ->
            False

        Entity e ->
            toUuid e == toUuid entity


compare : Identifiable -> String
compare i =
    case i of
        Entity e ->
            "Entity " ++ EN.compare e

        EntityType et ->
            "EntityType " ++ ENT.compare et


encode : Identifiable -> Encode.Value
encode i =
    case i of
        Entity e ->
            EN.encode e

        EntityType et ->
            ENT.encode et


decoder : Decoder Identifiable
decoder =
    -- some kind of intrusion into EN and ENT decoders
    Decode.field "what" Decode.string
        |> Decode.andThen
            (\w ->
                if (String.slice 0 4 <| String.reverse w) == "Type" then
                    Decode.map EntityType ENT.decoder

                else
                    Decode.map Entity EN.decoder
            )
