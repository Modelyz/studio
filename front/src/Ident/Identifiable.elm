module Ident.Identifiable exposing (Identifiable, hWithIdentifiers, tWithIdentifiers)

import Dict exposing (Dict)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Ident.Identifier as Identifier exposing (Identifier, fromHierarchic, fromTyped)
import Item.Item as Item exposing (Item)
import Prng.Uuid as Uuid exposing (Uuid)
import Typed.Typed exposing (Typed)


type alias Identifiable a =
    -- the shape of an identifiable
    { a | identifiers : Dict String String }


tWithIdentifiers : Dict String Identifier -> Dict String (Typed (Identifiable a)) -> Dict String (Typed (Identifiable a))
tWithIdentifiers identifiers ts =
    -- enrich the set of items with their identifiers as a dict
    Dict.map
        (\_ t -> { t | identifiers = fromTyped t identifiers |> Identifier.toDict })
        ts


hWithIdentifiers : Dict String Identifier -> Dict String (Hierarchic (Identifiable a)) -> Dict String (Hierarchic (Identifiable a))
hWithIdentifiers identifiers hs =
    -- enrich the set of items with their identifiers as a dict
    Dict.map
        (\_ h ->
            { h
                | identifiers = fromHierarchic h identifiers |> Identifier.toDict
            }
        )
        hs
