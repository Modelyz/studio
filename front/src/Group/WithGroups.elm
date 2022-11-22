module Group.WithGroups exposing (getGroups)

import Dict exposing (Dict)
import Group.Link as GroupLink
import Prng.Uuid exposing (Uuid)



-- TODO rename or move to State or Groups or Group.State


getGroups : Dict String GroupLink.Link -> Uuid -> List Uuid
getGroups links uuid =
    links
        |> Dict.filter (\_ link -> link.groupable == uuid)
        |> Dict.values
        |> List.map .group
