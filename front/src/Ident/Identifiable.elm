module Ident.Identifiable exposing (Identifiable, display, hWithIdentifiers, tWithIdentifiers, withIdentifiers)

import Configuration exposing (Configuration(..), getMostSpecific)
import Dict exposing (Dict)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Ident.Identifier as Identifier exposing (Identifier, fromUuid)
import Item.Item as Item exposing (Item)
import Prng.Uuid as Uuid exposing (Uuid)
import Typed.Typed exposing (Typed)
import Zone.Fragment exposing (displayFromDict)


type alias Identifiable a =
    -- the shape of an identifiable
    { a | identifiers : Dict String String }


tWithIdentifiers : Dict String Identifier -> Dict String (Typed (Identifiable a)) -> Dict String (Typed (Identifiable a))
tWithIdentifiers allIds ts =
    -- enrich the set of items with their identifiers as a dict
    -- TODO remove tWithIdentifiers and hWithIdentifiers and replace with just withIdentifiers
    Dict.map (\_ t -> withIdentifiers allIds t.uuid t) ts


hWithIdentifiers : Dict String Identifier -> Dict String (Hierarchic (Identifiable a)) -> Dict String (Hierarchic (Identifiable a))
hWithIdentifiers allIds hs =
    -- enrich the set of items with their identifiers as a dict
    Dict.map (\_ h -> withIdentifiers allIds h.uuid h) hs


withIdentifiers : Dict String Identifier -> Uuid -> Identifiable a -> Identifiable a
withIdentifiers allIds uuid i =
    { i | identifiers = fromUuid uuid allIds |> Identifier.toDict }


display : Maybe Configuration -> Identifiable (Item b) -> String
display mc hitem =
    mc
        |> Maybe.map (\(ZoneConfig _ fragments _) -> displayFromDict hitem.identifiers fragments)
        |> Maybe.withDefault (Uuid.toString hitem.uuid)
