module Group.Link exposing (Link, compare)

import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable exposing (Groupable)


type alias Link =
    -- the groupable is in the group
    { groupable : Groupable
    , group : Group
    }


compare : Link -> String
compare eg =
    Groupable.compare eg.groupable ++ Group.compare eg.group
