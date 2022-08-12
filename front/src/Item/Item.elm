module Item.Item exposing (Item, OnlyItem, compare, find)

import Dict exposing (Dict)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias OnlyItem =
    { what : Type, uuid : Uuid }


type alias Item a =
    { a | what : Type, uuid : Uuid }


find : Dict String (Item a) -> Uuid -> Maybe (Item a)
find es uuid =
    Dict.filter (\_ e -> e.uuid == uuid) es
        |> Dict.values
        |> List.head


compare : Item a -> String
compare =
    .uuid >> Uuid.toString
