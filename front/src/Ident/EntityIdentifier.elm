module Ident.EntityIdentifier exposing (..)

import DateTime exposing (..)
import DictSet as Set exposing (DictSet)
import Element exposing (..)
import Ident.Identifiable as Identifiable exposing (Identifiable, isRelated)
import Ident.Identifier as Identifier exposing (Identifier)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid
import REA.Entity as EN exposing (Entity(..), toUuid)
import REA.EntityType as ENT exposing (toName)
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)


type alias EntityIdentifier =
    -- the link between an identifiable and its identifier
    { identifiable : Identifiable
    , identifier : Identifier
    }


select : Entity -> String -> DictSet String EntityIdentifier -> Maybe EntityIdentifier
select entity name =
    Set.filter (\i -> i.identifier.name == name && isRelated entity i.identifiable) >> Set.toList >> List.head


compare : EntityIdentifier -> String
compare ei =
    Identifiable.compare ei.identifiable ++ ei.identifier.name


encode : EntityIdentifier -> Encode.Value
encode ei =
    Encode.object
        [ ( "identifiable", Identifiable.encode ei.identifiable )
        , ( "identifier", Identifier.encode ei.identifier )
        ]


decoder : Decoder EntityIdentifier
decoder =
    Decode.map2 EntityIdentifier
        (Decode.field "identifiable" Identifiable.decoder)
        (Decode.field "identifier" Identifier.decoder)
