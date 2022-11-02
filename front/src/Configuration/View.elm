module Configuration.View exposing (description, view)

import Configuration exposing (Configuration(..))
import Element exposing (..)
import Ident.Identifiable exposing (withIdentifiers)
import Scope.Scope as Scope
import Scope.View
import Shared
import State exposing (allHfromScope, allTfromScope)
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
            Scope.View.toDisplay
                (allTfromScope s.state scope
                    |> withIdentifiers s.state
                )
                (allHfromScope s.state scope |> withIdentifiers s.state)
                s.state.configs
                scope
