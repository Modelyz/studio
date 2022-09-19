module Ident.Identifiable exposing (Identifiable, display, hWithIdentifiers, tWithIdentifiers, withIdentifiers)

import Configuration exposing (Configuration(..), getMostSpecific)
import Dict exposing (Dict)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Ident.Identifier as Identifier exposing (Identifier, fromUuid)
import Item.Item as Item exposing (Item)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Typed.Typed exposing (Typed)
import Zone.Fragment exposing (displayFromDict)


type alias Identifiable a =
    -- the shape of an identifiable
    { a | identifiers : Dict String Identifier }


tWithIdentifiers : Dict String Identifier -> Dict String (Typed a) -> Dict String (Typed a)
tWithIdentifiers allIds ts =
    -- enrich the set of items with their identifiers as a dict
    -- TODO remove tWithIdentifiers and hWithIdentifiers and replace with just withIdentifiers
    Dict.map (\_ t -> withIdentifiers allIds t.what t.uuid t) ts


hWithIdentifiers : Dict String Identifier -> Dict String (Hierarchic a) -> Dict String (Hierarchic a)
hWithIdentifiers allIds hs =
    -- enrich the set of items with their identifiers as a dict
    Dict.map (\_ h -> withIdentifiers allIds h.what h.uuid h) hs


withIdentifiers : Dict String Identifier -> Type -> Uuid -> Identifiable a -> Identifiable a
withIdentifiers allIds what uuid i =
    -- TODO only take i as arg and add what and uuid in Identifiable
    { i | identifiers = allIds |> Dict.filter (\_ id -> uuid == id.identifiable) }


display : Maybe Configuration -> Identifiable (Item b) -> String
display mc hitem =
    mc
        |> Maybe.map (\(ZoneConfig _ fragments _) -> displayFromDict hitem.identifiers fragments)
        |> Maybe.withDefault (Uuid.toString hitem.uuid)
