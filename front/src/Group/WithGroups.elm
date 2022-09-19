module Group.WithGroups exposing (WithGroups, withGroups)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Group.Groupable as Groupable
import Group.Link as GroupLink
import Item.Item exposing (Item)
import Prng.Uuid as Uuid


type alias WithGroups a =
    { a | groups : Dict String Group }


withGroups : Dict String GroupLink.Link -> WithGroups (Item a) -> WithGroups (Item a)
withGroups gls i =
    { i
        | groups =
            gls
                |> Dict.filter (\_ gl -> Groupable.uuid gl.groupable == i.uuid)
                |> Dict.values
                |> List.map (\gl -> ( gl.group.uuid |> Uuid.toString, gl.group ))
                |> Dict.fromList
    }
