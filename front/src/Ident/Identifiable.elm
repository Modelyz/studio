module Ident.Identifiable exposing (Identifiable, hWithIdentifiers, tWithIdentifiers, withIdentifiers)

import Configuration exposing (Configuration(..), getMostSpecific)
import Dict exposing (Dict)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Ident.Identifier as Identifier exposing (Identifier, fromUuid)
import Prng.Uuid as Uuid exposing (Uuid)
import Typed.Typed exposing (Typed)


type alias Identifiable a =
    -- the shape of an identifiable
    { a | uuid : Uuid, identifiers : Dict String Identifier }


tWithIdentifiers : Dict String Identifier -> Dict String (Typed a) -> Dict String (Typed a)
tWithIdentifiers allIds ts =
    -- enrich the set of items with their identifiers as a dict
    -- TODO remove tWithIdentifiers and hWithIdentifiers and replace with just withIdentifiers
    Dict.map (\_ t -> withIdentifiers allIds t) ts


hWithIdentifiers : Dict String Identifier -> Dict String (Hierarchic a) -> Dict String (Hierarchic a)
hWithIdentifiers allIds hs =
    -- enrich the set of items with their identifiers as a dict
    Dict.map (\_ h -> withIdentifiers allIds h) hs


withIdentifiers : Dict String Identifier -> Identifiable a -> Identifiable a
withIdentifiers allIds i =
    { i | identifiers = allIds |> Dict.filter (\_ id -> i.uuid == id.identifiable) }
