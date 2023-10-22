module Configuration.Zone.View exposing (displayZone)

import Configuration as Config exposing (Configuration(..))
import Configuration.Zone exposing (Zone(..))
import Configuration.Zone.Fragment exposing (Fragment(..))
import Dict
import Element exposing (..)
import Expression.Eval as Eval
import Expression.Rational as Rational
import Flow exposing (Flow(..))
import Group.Group as Group
import Hierarchy.Type as HType
import Ident.Identifier as Identifier
import Prng.Uuid as Uuid exposing (Uuid)
import Scope exposing (Scope(..))
import State exposing (State)
import Type exposing (Type)
import Typed.Type as TType
import Util exposing (ifEmpty)
import View.Style exposing (..)


displayZone : State -> Zone -> Type -> Uuid -> String
displayZone s zone t uuid =
    -- TODO replace types configs allIds allGroupLinks with just s
    Config.getMostSpecific s.types s.configs zone (IsItem t uuid)
        |> Maybe.map
            (\conf ->
                case conf of
                    ZoneDisplay display ->
                        String.concat <| List.map (toValue s t uuid zone) display.fragments

                    _ ->
                        ""
            )
        |> Maybe.withDefault (Uuid.toString uuid)


toValue : State -> Type -> Uuid -> Zone -> Fragment -> String
toValue s t uuid zone fragment =
    case fragment of
        IdentifierName identifier ->
            Identifier.select identifier.name (Identifier.fromUuid uuid s.identifiers) |> List.map Identifier.toValue |> String.join ", "

        GroupIdentifierName groupidentifier ->
            let
                groupids =
                    Group.groupsOf s.grouped uuid |> List.map (\guuid -> Identifier.fromUuid guuid s.identifiers |> Dict.toList) |> List.concat |> Dict.fromList
            in
            Identifier.select groupidentifier.name groupids |> List.map Identifier.toValue |> String.join ", "

        Parent ->
            let
                parent =
                    Dict.get (Uuid.toString uuid) s.groups |> Maybe.andThen .parent
            in
            Maybe.map (displayZone s zone (Type.TType TType.Group)) parent |> Maybe.withDefault ""

        Fixed string ->
            string

        Quantity ->
            case t of
                Type.TType TType.Commitment ->
                    Dict.get (Uuid.toString uuid) s.commitments
                        |> Maybe.map (.qty >> Eval.exeval s { context = ( t, uuid ) } s.values >> Rational.resultToString)
                        |> Maybe.withDefault ""

                Type.TType TType.Event ->
                    Dict.get (Uuid.toString uuid) s.events
                        |> Maybe.map (.qty >> Eval.exeval s { context = ( t, uuid ) } s.values >> Rational.resultToString)
                        |> Maybe.withDefault ""

                _ ->
                    ""

        Flow ->
            (case t of
                Type.TType TType.Commitment ->
                    Dict.get (Uuid.toString uuid) s.commitments

                Type.TType TType.Event ->
                    Dict.get (Uuid.toString uuid) s.events

                _ ->
                    Nothing
            )
                |> Maybe.map
                    (.flow
                        >> (\f ->
                                case f of
                                    ResourceFlow resource ->
                                        displayZone s zone (Type.TType TType.Resource) resource.uuid

                                    ResourceTypeFlow resourceType ->
                                        displayZone s zone (Type.HType HType.ResourceType) resourceType.uuid
                           )
                    )
                |> Maybe.withDefault ""

        Provider ->
            (case t of
                Type.TType TType.Commitment ->
                    Dict.get (Uuid.toString uuid) s.commitments

                Type.TType TType.Event ->
                    Dict.get (Uuid.toString uuid) s.events

                _ ->
                    Nothing
            )
                |> Maybe.map
                    (.provider
                        >> displayZone s zone (Type.TType TType.Agent)
                    )
                |> Maybe.withDefault ""

        Receiver ->
            (case t of
                Type.TType TType.Commitment ->
                    Dict.get (Uuid.toString uuid) s.commitments

                Type.TType TType.Event ->
                    Dict.get (Uuid.toString uuid) s.events

                _ ->
                    Nothing
            )
                |> Maybe.map
                    (.receiver
                        >> displayZone s zone (Type.TType TType.Agent)
                    )
                |> Maybe.withDefault ""

        EventList separator ->
            case t of
                Type.TType TType.Process ->
                    Dict.filter (\_ r -> r.process == uuid) s.reconciliations
                        |> Dict.values
                        |> List.map (\r -> Rational.toFloatString r.qty ++ separator.qtySep ++ displayZone s zone (Type.TType TType.Event) r.event)
                        |> String.join separator.eventSep
                        |> ifEmpty "(empty)"

                _ ->
                    ""
