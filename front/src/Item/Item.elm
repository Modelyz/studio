module Item.Item exposing (Item, OnlyItem, compare, find)

import DictSet as Set exposing (DictSet)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias OnlyItem =
    { what : Type, uuid : Uuid }


type alias Item a =
    { a | what : Type, uuid : Uuid }


find : DictSet String (Item a) -> Uuid -> Maybe (Item a)
find es uuid =
    Set.filter (\e -> e.uuid == uuid) es
        |> Set.toList
        |> List.head


compare : Item a -> String
compare =
    .uuid >> Uuid.toString
