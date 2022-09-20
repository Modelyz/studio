module Item.Item exposing (Item, find)

import Dict exposing (Dict)
import Prng.Uuid exposing (Uuid)
import Type exposing (Type)


type alias Item a =
    -- TODO could "what" be removed by turning all the items into a non-alias type?
    { a | what : Type, uuid : Uuid }


find : Dict String (Item a) -> Uuid -> Maybe (Item a)
find es uuid =
    Dict.filter (\_ e -> e.uuid == uuid) es
        |> Dict.values
        |> List.head
