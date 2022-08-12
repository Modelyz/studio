module Restriction.Restriction exposing (Restriction, compare, decoder, encode)

import Dict exposing (Dict)
import Entity.Entity as Entity exposing (Entity, isChildOf, isChildOfAny)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Represent the link between two entity types, the what restricted by the scope
-- TODO replace with a Typed Link? (The type being the event)


type alias Restriction =
    { what : Entity
    , scope : Entity
    }


compare : Restriction -> String
compare r =
    Entity.compare r.what ++ "_" ++ Entity.compare r.scope


encode : Restriction -> Encode.Value
encode r =
    Encode.object
        [ ( "what", Entity.encode r.what )
        , ( "scope", Entity.encode r.scope )
        ]


decoder : Decoder Restriction
decoder =
    Decode.map2 Restriction
        (Decode.field "what" Entity.decoder)
        (Decode.field "scope" Entity.decoder)


restrictBy : Dict String Entity -> Dict String Restriction -> Entity -> Dict String Entity -> Dict String Entity
restrictBy entityTypes restrictions scope ets =
    -- keep only the entityTypes which are children of the entityTypes of the scopes which are children of the given scope
    -- TODO cleanup? was used to restrict some entities (such as events) to certain process types
    let
        parentTypes =
            restrictions
                |> Dict.map (\_ r -> r.scope)
                |> Dict.filter (\_ s -> isChildOf scope entityTypes s)
    in
    ets |> Dict.filter (\_ et -> isChildOfAny entityTypes parentTypes et)
