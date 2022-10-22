module Item.Item exposing (Item, find)

import Dict exposing (Dict)
import Ident.Identifier exposing (Identifier)
import Prng.Uuid exposing (Uuid)
import Type exposing (Type)
import Typed.Type as TType
import Value.Value exposing (Value)


type alias Item a =
    -- TODO could "what" be removed by turning all the items into a non-alias type?
    { a
        | what : Type
        , uuid : Uuid
        , identifiers : Dict String Identifier
        , values : Dict String Value
        , display : Dict String String
    }


find : Dict String (Item a) -> Uuid -> Maybe (Item a)
find es uuid =
    Dict.filter (\_ e -> e.uuid == uuid) es
        |> Dict.values
        |> List.head
