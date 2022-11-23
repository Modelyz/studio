module Configuration.View exposing (description, view)

import Configuration exposing (Configuration(..))
import Scope.View
import Shared
import Zone.Fragment as Fragment
import Zone.Zone as Zone


view : Shared.Model -> Configuration -> String
view s c =
    -- TODO convert to a separate title and description to feed in to a smallcard
    case c of
        ZoneConfig zone fragments _ ->
            Zone.toDesc zone
                ++ " : "
                ++ (String.concat <| List.map Fragment.toString fragments)


description : Shared.Model -> Configuration -> String
description s c =
    case c of
        ZoneConfig _ _ scope ->
            Scope.View.toDisplay s scope
