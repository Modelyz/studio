module Group.WithGroups exposing (WithGroups, hWithGroups, tWithGroups)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Group.Groupable as Groupable
import Group.Link as GroupLink
import Hierarchy.Hierarchic exposing (Hierarchic)
import Item.Item exposing (Item)
import Prng.Uuid as Uuid
import Typed.Typed exposing (Typed)


type alias WithGroups a =
    { a | groups : Dict String Group }


tWithGroups : Dict String GroupLink.Link -> WithGroups (Typed a) -> WithGroups (Typed a)
tWithGroups gls i =
    { i
        | groups =
            gls
                |> Dict.filter (\_ gl -> Groupable.uuid gl.groupable == i.uuid)
                |> Dict.values
                |> List.map (\gl -> ( gl.group.uuid |> Uuid.toString, gl.group ))
                |> Dict.fromList
    }


hWithGroups : Dict String GroupLink.Link -> WithGroups (Hierarchic a) -> WithGroups (Hierarchic a)
hWithGroups gls i =
    { i
        | groups =
            gls
                |> Dict.filter (\_ gl -> Groupable.uuid gl.groupable == i.uuid)
                |> Dict.values
                |> List.map (\gl -> ( gl.group.uuid |> Uuid.toString, gl.group ))
                |> Dict.fromList
    }
