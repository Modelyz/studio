module Zone.View exposing (display)

import Configuration as Config exposing (Configuration(..))
import Dict exposing (Dict)
import Group.Group as Group
import Group.Link exposing (Link)
import Ident.Identifier as Identifier exposing (Identifier)
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope exposing (Scope(..))
import Type exposing (Type)
import Zone.Fragment as ZoneFragment
import Zone.Zone exposing (Zone)


display : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String Configuration -> Zone -> Dict String Identifier -> Dict String Link -> Type -> Uuid -> String
display types configs zone allIds allGroupLinks t uuid =
    -- TODO replace types configs allIds allGroupLinks with just s
    Config.getMostSpecific types configs zone (IsItem t uuid)
        |> Maybe.map
            (\(ZoneConfig _ fragments _) ->
                ZoneFragment.display
                    (Identifier.fromUuid uuid allIds)
                    (List.map (\guuid -> Identifier.fromUuid guuid allIds |> Dict.toList) (Group.groupsOf allGroupLinks uuid) |> List.concat |> Dict.fromList)
                    fragments
            )
        |> Maybe.withDefault (Uuid.toString uuid)
