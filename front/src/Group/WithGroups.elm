module Group.WithGroups exposing (getGroups)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Group.Groupable as Groupable
import Group.Link as GroupLink
import Hierarchy.Hierarchic exposing (Hierarchic)
import Item.Item exposing (Item)
import Prng.Uuid as Uuid exposing (Uuid)
import Typed.Typed exposing (Typed)



-- TODO rename or move to State or Groups or Group.State


getGroups : Dict String GroupLink.Link -> Uuid -> List Uuid
getGroups links uuid =
    links
        |> Dict.filter (\_ link -> link.groupable == uuid)
        |> Dict.values
        |> List.map .group
