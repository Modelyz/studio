module Ident.Identifiable exposing (Identifiable, hWithIdentifiers, tWithIdentifiers)

import Dict exposing (Dict)
import Hierarchy.Hierarchic as H exposing (Hierarchic, OnlyHierarchic)
import Ident.Identifier as Identifier exposing (Identifier, fromHierarchic, fromTyped)
import Item.Item as Item exposing (Item)
import Prng.Uuid as Uuid exposing (Uuid)
import Typed.Typed as T exposing (OnlyTyped, Typed)


type alias Identifiable a =
    -- the shape of an identifiable
    { a | identifiers : Dict String String }


tWithIdentifiers : Dict String Identifier -> Dict String (Typed a) -> Dict String (Identifiable OnlyTyped)
tWithIdentifiers identifiers items =
    -- enrich the set of items with their identifiers as a dict
    Dict.map
        (\_ item ->
            { what = item.what
            , uuid = item.uuid
            , type_ = item.type_
            , identifiers = fromTyped item identifiers |> Identifier.toDict
            }
        )
        items


hWithIdentifiers : Dict String Identifier -> Dict String (Hierarchic a) -> Dict String (Identifiable OnlyHierarchic)
hWithIdentifiers identifiers items =
    -- enrich the set of items with their identifiers as a dict
    Dict.map
        (\_ item ->
            { what = item.what
            , uuid = item.uuid
            , parent = item.parent
            , identifiers = fromHierarchic item identifiers |> Identifier.toDict
            }
        )
        items
