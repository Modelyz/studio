module Configuration.View exposing (description, view)

import Configuration exposing (Configuration(..))
import Element exposing (..)
import Scope.Scope as Scope
import Scope.View
import Shared
import Zone.Fragment as Fragment exposing (display)
import Zone.Zone as Zone


view : Shared.Model -> Configuration -> String
view s c =
    -- TODO convert to a separate title and description to feed in to a smallcard
    case c of
        ZoneConfig zone fragments scope ->
            Zone.toDesc zone
                ++ " : "
                ++ (String.join "" <| List.map Fragment.toString fragments)


description : Shared.Model -> Configuration -> String
description s c =
    case c of
        ZoneConfig zone fragments scope ->
            Scope.View.toDisplay s.state.types s.state.identifiers s.state.configs scope
