module Zone.View exposing (displayZone)

import Configuration as Config exposing (Configuration(..))
import Dict exposing (Dict)
import Group.Group as Group exposing (Group)
import Group.Link exposing (Link)
import Ident.Identifier as Identifier exposing (Identifier)
import Prng.Uuid as Uuid exposing (Uuid)
import Scope exposing (Scope(..))
import State exposing (State)
import Tree
import Type exposing (Type)
import Typed.Type as TType
import Util exposing (third)
import Zone.Fragment as ZoneFragment exposing (Fragment(..))
import Zone.Zone exposing (Zone(..))


displayZone : State -> Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String Configuration -> Zone -> Dict String Identifier -> Dict String Link -> Dict String Group -> Type -> Uuid -> String
displayZone state types configs zone allIds allGroupLinks allGroups t uuid =
    -- TODO replace types configs allIds allGroupLinks with just s
    Config.getMostSpecific types configs zone (IsItem t uuid)
        |> Maybe.map
            (\(ZoneConfig _ fragments _) ->
                String.concat <|
                    List.map
                        (toValue
                            state
                            zone
                            (Identifier.fromUuid uuid allIds)
                            (List.map (\guuid -> Identifier.fromUuid guuid allIds |> Dict.toList) (Group.groupsOf allGroupLinks uuid) |> List.concat |> Dict.fromList)
                            (Dict.get (Uuid.toString uuid) allGroups |> Maybe.andThen .parent)
                        )
                        fragments
            )
        |> Maybe.withDefault (Uuid.toString uuid)


toValue : State -> Zone -> Dict String Identifier -> Dict String Identifier -> Maybe Uuid -> Fragment -> String
toValue state zone identifiers groupids parent fragment =
    case fragment of
        IdentifierName name ->
            Identifier.select name identifiers |> List.map Identifier.toValue |> String.join ", "

        GroupIdentifierName name ->
            Identifier.select name groupids |> List.map Identifier.toValue |> String.join ", "

        Parent ->
            Maybe.map (displayZone state state.types state.configs zone state.identifiers state.grouped state.groups (Type.TType TType.Group)) parent |> Maybe.withDefault ""

        Fixed string ->
            string
