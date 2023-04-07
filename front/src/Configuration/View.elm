module Configuration.View exposing (description, view)

import Configuration exposing (Configuration(..))
import Scope.View
import Shared
import Zone.Fragment as Fragment
import Zone.Zone as Zone


view : Configuration -> String
view c =
    -- TODO convert to a separate title and description to feed in to a smallcard
    case c of
        ZoneDisplay zone fragments _ ->
            Zone.toDesc zone
                ++ " : "
                ++ (String.concat <| List.map Fragment.toString fragments)


description : Shared.Model -> Configuration -> String
description s c =
    case c of
        ZoneDisplay _ _ scope ->
            Scope.View.toDisplay s.state scope
