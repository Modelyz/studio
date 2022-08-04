module Link.Link exposing (Link(..), compare)

import Entity.Entity as Entity exposing (Entity)
import Group.Group as Group exposing (Group)


type Link
    = EntityGroup Entity Group


compare : Link -> String
compare l =
    case l of
        EntityGroup e g ->
            Entity.compare e ++ " " ++ Group.compare g
