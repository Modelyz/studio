module Expression.Eval exposing (Config, dleval, exeval, veval)

import Agent.Agent as Agent
import Dict exposing (Dict)
import Expression exposing (..)
import Expression.Binary as B
import Expression.DeepLink exposing (DeepLink(..))
import Expression.HardLink as HardLink exposing (HardLink(..))
import Expression.Observable exposing (Observable(..))
import Expression.Rational as Rational exposing (Rational)
import Expression.Unary as U
import Expression.ValueSelection exposing (ValueSelection(..))
import Flow
import Group.Group as Group
import Prng.Uuid exposing (Uuid)
import Scope exposing (Scope(..), toType)
import Scope.State exposing (containsScope)
import State exposing (State)
import Tree exposing (parentOf)
import Type exposing (Type, typeOf)
import Util exposing (chooseIfSingleton)
import Value.Value exposing (..)


type alias Config =
    { context : ( Type, Uuid ) }


veval : State -> Config -> Dict String Value -> Value -> Result String Rational
veval s c allVals value =
    exeval s c allVals value.expr


exeval : State -> Config -> Dict String Value -> Expression -> Result String Rational
exeval s c allVals expr =
    case expr of
        Leaf obs ->
            case obs of
                ObsNumber n ->
                    Rational.fromString n.input

                ObsValue vs ->
                    case vs of
                        UndefinedValue ->
                            Err "Undefined"

                        SelectedValue w f n ->
                            allVals
                                |> Dict.filter (\_ x -> x.what == w && x.for == f && x.name == n)
                                |> Dict.values
                                |> List.head
                                |> Result.fromMaybe "The value does not exist anymore"
                                |> Result.andThen (.expr >> exeval s c allVals)

                ObsLink deeplink ->
                    -- Tuple.second c.context est le uuid, qui n'existe pas. Donc l'appel de step ensuite est vide
                    dleval s deeplink [ Tuple.second c.context ]
                        |> chooseIfSingleton
                        |> Maybe.map (veval s c s.values)
                        |> Maybe.withDefault (Err "No result")

        Unary op e ->
            Result.map (U.eval op) (exeval s c allVals e)

        Binary op e f ->
            -- the error is displayed only for the 1st eval even if both fail
            B.eval op (exeval s c allVals e) (exeval s c allVals f)


step : State -> HardLink -> Uuid -> List Uuid
step s hl uuid =
    {- For each hardlink encountered in the deeplink,
       return the list of entity uuids corresponding to the target of the hardlink
    -}
    case hl of
        ResourceLink HardLink.ResourceGroup ->
            Group.groupsOf s.grouped uuid

        ResourceLink HardLink.ResourceType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        EventLink HardLink.EventProvider ->
            Agent.providerOf s.events s.agents uuid

        EventLink HardLink.EventReceiver ->
            Agent.receiverOf s.events s.agents uuid

        EventLink HardLink.EventInflow ->
            Flow.flowOf s.events s.resources s.resourceTypes uuid

        EventLink HardLink.EventOutflow ->
            Flow.flowOf s.events s.resources s.resourceTypes uuid

        EventLink HardLink.EventGroup ->
            Group.groupsOf s.grouped uuid

        EventLink HardLink.EventType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        AgentLink HardLink.AgentGroup ->
            Group.groupsOf s.grouped uuid

        AgentLink HardLink.AgentType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        CommitmentLink HardLink.CommitmentProvider ->
            Agent.providerOf s.commitments s.agents uuid

        CommitmentLink HardLink.CommitmentReceiver ->
            Agent.receiverOf s.commitments s.agents uuid

        CommitmentLink HardLink.CommitmentInflow ->
            Flow.flowOf s.events s.resources s.resourceTypes uuid

        CommitmentLink HardLink.CommitmentOutflow ->
            Flow.flowOf s.events s.resources s.resourceTypes uuid

        CommitmentLink HardLink.CommitmentGroup ->
            Group.groupsOf s.grouped uuid

        CommitmentLink HardLink.CommitmentType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        ContractLink HardLink.ContractGroup ->
            Group.groupsOf s.grouped uuid

        ContractLink HardLink.ContractType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        ProcessLink HardLink.ProcessGroup ->
            Group.groupsOf s.grouped uuid

        ProcessLink HardLink.ProcessType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        GroupLink HardLink.ParentGroup ->
            Maybe.withDefault [] <| Maybe.map List.singleton (parentOf s.groups uuid)

        GroupLink HardLink.GroupGroup ->
            Group.groupsOf s.grouped uuid

        GroupLink HardLink.GroupType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        ResourceTypeLink HardLink.ResourceTypeGroup ->
            Group.groupsOf s.grouped uuid

        ResourceTypeLink HardLink.ResourceTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        EventTypeLink HardLink.EventTypeReceiver ->
            Agent.receiversOf s.types s.eventTypes s.agents uuid

        EventTypeLink HardLink.EventTypeProvider ->
            Agent.providersOf s.types s.eventTypes s.agents uuid

        EventTypeLink HardLink.EventTypeGroup ->
            Group.groupsOf s.grouped uuid

        EventTypeLink HardLink.EventTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        AgentTypeLink HardLink.AgentTypeGroup ->
            Group.groupsOf s.grouped uuid

        AgentTypeLink HardLink.AgentTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        CommitmentTypeLink HardLink.CommitmentTypeReceiver ->
            Agent.receiversOf s.types s.commitmentTypes s.agents uuid

        CommitmentTypeLink HardLink.CommitmentTypeProvider ->
            Agent.providersOf s.types s.commitmentTypes s.agents uuid

        CommitmentTypeLink HardLink.CommitmentTypeGroup ->
            Group.groupsOf s.grouped uuid

        CommitmentTypeLink HardLink.CommitmentTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        ContractTypeLink HardLink.ContractTypeGroup ->
            Group.groupsOf s.grouped uuid

        ContractTypeLink HardLink.ContractTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        ProcessTypeLink HardLink.ProcessTypeGroup ->
            Group.groupsOf s.grouped uuid

        ProcessTypeLink HardLink.ProcessTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        GroupTypeLink HardLink.GroupTypeGroup ->
            Group.groupsOf s.grouped uuid

        GroupTypeLink HardLink.GroupTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.types uuid)

        _ ->
            -- FIXME missing EventTypeFlow and CommitmentTypeFlow
            []


dleval : State -> DeepLink -> List Uuid -> List Value
dleval s deeplink currentlist =
    {- Recursive function that recompute each new list of uuid at each level of the deeplink,
       while transmitting the resulting uuid list to the endpoint, so that the final list of value can be computed
    -}
    case deeplink of
        Null ->
            -- end immediately
            []

        Link hardlink nextDeeplink ->
            -- run the eval one step further
            dleval s nextDeeplink (List.map (\uuid -> step s hardlink uuid) currentlist |> List.concat)

        EndPoint scope name ->
            -- use the transmitted list of uuid and find the corresponding values
            -- FIXME TODO not only groups
            let
                groups =
                    currentlist |> List.filter (\u -> toType scope |> Maybe.map (\t -> containsScope s.types (IsItem t u) scope) |> Maybe.withDefault False)
            in
            List.map (\g -> s.values |> Dict.filter (\_ v -> v.for == g && v.name == name) |> Dict.values) groups |> List.concat
