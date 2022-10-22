module Configuration.View exposing (display)

import Configuration exposing (Configuration(..))
import Element exposing (..)
import Ident.Identifiable exposing (withIdentifiers)
import Scope.Scope as Scope
import Scope.View
import Shared
import State exposing (allHfromScope, allTfromScope)
import Zone.Fragment as Fragment exposing (display)
import Zone.Zone as Zone


display : Shared.Model -> Configuration -> Element msg
display s c =
    -- TODO convert to a separate title and description to feed in to a smallcard
    case c of
        ZoneConfig zone fragments scope ->
            column [ spacing 10 ]
                [ text <|
                    Zone.toDesc zone
                        ++ " : "
                        ++ (String.join "" <| List.map Fragment.toString (Debug.log "fragments" fragments))
                , text <|
                    " for "
                        ++ Scope.View.toDisplay
                            (allTfromScope s.state scope
                                |> withIdentifiers s.state
                            )
                            (allHfromScope s.state scope |> withIdentifiers s.state)
                            s.state.configs
                            scope
                ]
