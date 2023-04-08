module Configuration.View exposing (description, view)

import Configuration exposing (Configuration(..))
import Prng.Uuid as Uuid
import Scope.View
import Shared
import Type
import Configuration.Zone.Fragment as Fragment
import Configuration.Zone.View exposing (displayZone)
import Configuration.Zone as Zone exposing (Zone(..))


view : Configuration -> String
view c =
    -- TODO convert to a separate title and description to feed in to a smallcard
    case c of
        ZoneDisplay zone fragments _ ->
            Zone.toDesc zone
                ++ " : "
                ++ (String.concat <| List.map Fragment.toString fragments)

        MenuDisplay type_ uuid hasMenu ->
            Type.toString type_
                ++ " "
                ++ Uuid.toString uuid
                ++ ": "
                ++ (if hasMenu then
                        "visible"

                    else
                        "not visible"
                   )


description : Shared.Model -> Configuration -> String
description s c =
    case c of
        ZoneDisplay _ _ scope ->
            Scope.View.toDisplay s.state scope

        MenuDisplay type_ uuid hasMenu ->
            displayZone s.state SmallcardTitle type_ uuid
                ++ ": "
                ++ (if hasMenu then
                        "visible"

                    else
                        "not visible"
                   )
