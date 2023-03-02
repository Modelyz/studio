module Zone.View exposing (allFragmentsByScope, displayZone)

import Configuration as Config exposing (Configuration(..))
import Dict
import Expression.Eval as Eval
import Expression.Rational as Rational
import Flow exposing (Flow(..))
import Group.Group as Group
import Hierarchy.Type as HType
import Ident.Identifier as Identifier
import Prng.Uuid as Uuid exposing (Uuid)
import Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import State exposing (State)
import Type exposing (Type)
import Typed.Type as TType
import Zone.Fragment exposing (Fragment(..))
import Zone.Zone exposing (Zone(..))


displayZone : State -> Zone -> Type -> Uuid -> String
displayZone s zone t uuid =
    -- TODO replace types configs allIds allGroupLinks with just s
    Config.getMostSpecific s.types s.configs zone (IsItem t uuid)
        |> Maybe.map
            (\(ZoneConfig _ fragments _) ->
                String.concat <|
                    List.map (toValue s t uuid zone) fragments
            )
        |> Maybe.withDefault (Uuid.toString uuid)


toValue : State -> Type -> Uuid -> Zone -> Fragment -> String
toValue s t uuid zone fragment =
    case fragment of
        IdentifierName name ->
            Identifier.select name (Identifier.fromUuid uuid s.identifiers) |> List.map Identifier.toValue |> String.join ", "

        GroupIdentifierName name ->
            let
                groupids =
                    List.map (\guuid -> Identifier.fromUuid guuid s.identifiers |> Dict.toList) (Group.groupsOf s.grouped uuid) |> List.concat |> Dict.fromList
            in
            Identifier.select name groupids |> List.map Identifier.toValue |> String.join ", "

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
                        |> Maybe.map (.qty >> Eval.exeval s { context = ( t, uuid ) } s.values >> Rational.toRString)
                        |> Maybe.withDefault ""

                Type.TType TType.Event ->
                    Dict.get (Uuid.toString uuid) s.events
                        |> Maybe.map (.qty >> Eval.exeval s { context = ( t, uuid ) } s.values >> Rational.toRString)
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


allFragmentsByScope : State -> Scope -> List Fragment
allFragmentsByScope s scope =
    -- all the relevant fragments for the scope
    let
        identifierNames =
            s.identifierTypes
                |> Dict.values
                |> List.filter
                    (\it -> containsScope s.types scope it.scope)
                |> List.map (.name >> IdentifierName)

        groupIdentifierNames =
            s.identifierTypes
                |> Dict.values
                |> List.filter
                    (\it -> containsScope s.types it.scope (HasType (Type.TType TType.Group)))
                |> List.map (.name >> GroupIdentifierName)
    in
    identifierNames
        ++ groupIdentifierNames
        ++ [ Fixed ""
           , Parent
           ]
        ++ (case scope of
                IsItem t _ ->
                    allByType t

                HasUserType t _ ->
                    allByType t

                HasType t ->
                    allByType t

                _ ->
                    []
           )


allByType : Type -> List Fragment
allByType t =
    case t of
        Type.TType TType.Event ->
            [ Quantity, Flow, Provider, Receiver ]

        Type.TType TType.Commitment ->
            [ Quantity, Flow, Provider, Receiver ]

        _ ->
            []
