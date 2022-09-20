module View.List exposing (Record, toRecord)

import Dict exposing (Dict)
import Group.Groupable as Groupable
import Group.Link as GroupLink
import Ident.Identifier exposing (Identifier)
import Item.Item exposing (Item)


type alias Record =
    { identifiers : Dict String Identifier
    , grouped : Dict String GroupLink.Link
    }


toRecord : Dict String Identifier -> Dict String GroupLink.Link -> Item a -> Record
toRecord allIds allGroupLinks i =
    { identifiers = allIds |> Dict.filter (\_ v -> v.identifiable == i.uuid)
    , grouped = allGroupLinks |> Dict.filter (\_ v -> Groupable.uuid v.groupable == i.uuid)
    }
