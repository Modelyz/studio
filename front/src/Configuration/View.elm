module Configuration.View exposing (description, view)

import Configuration exposing (Configuration(..))
import Configuration.Zone as Zone exposing (Zone(..))
import Configuration.Zone.Fragment as Fragment
import Configuration.Zone.View exposing (displayZone)
import Hierarchy.Type as HType
import Prng.Uuid as Uuid
import Scope.View
import Shared
import Type


view : Configuration -> String
view c =
    -- TODO convert to a separate title and description to feed in to a smallcard
    case c of
        ZoneDisplay display ->
            Zone.toDesc display.zone
                ++ " : "
                ++ (String.concat <| List.map Fragment.toString display.fragments)

        MenuDisplay type_ uuid hasMenu ->
            HType.toString type_
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
        ZoneDisplay display ->
            Scope.View.toDisplay s.state display.scope

        MenuDisplay type_ uuid hasMenu ->
            displayZone s.state SmallcardZone (Type.HType type_) uuid
                ++ ": "
                ++ (if hasMenu then
                        "visible"

                    else
                        "not visible"
                   )
