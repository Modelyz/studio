module Expression.Eval exposing (Config, dleval, exeval, veval)

import Agent.Agent as Agent
import Dict exposing (Dict)
import Expression exposing (..)
import Expression.Binary as B
import Expression.DeepLink as DeepLink exposing (DeepLink(..))
import Expression.HardLink as HardLink exposing (HardLink(..))
import Expression.Observable exposing (Observable(..))
import Expression.Rational as Rational exposing (Rational)
import Expression.Unary as U
import Expression.ValueSelection as ValueSelection exposing (ValueSelection(..))
import Flow
import Group.Group as Group
import Prng.Uuid as Uuid exposing (Uuid)
import Resource.Resource as Resource
import Scope.Scope exposing (Scope(..), toType)
import Scope.State exposing (containsScope)
import Shared
import Tree exposing (parentOf)
import Type exposing (Type, typeOf)
import Typed.Type as TType
import Util exposing (chooseIfSingleton)
import Value.Value exposing (..)


type alias Config =
    { context : ( Type, Uuid ) }


veval : Shared.Model -> Config -> Dict String Value -> Value -> Result String Rational
veval s c allVals value =
    exeval s c allVals value.expr


exeval : Shared.Model -> Config -> Dict String Value -> Expression -> Result String Rational
exeval s c allVals expr =
    case expr of
        Leaf obs ->
            case obs of
                ObsNumber n ->
                    n.val

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
                        |> Maybe.map (veval s c s.state.values)
                        |> Maybe.withDefault (Err "No result")

        Unary op e ->
            Result.map (U.eval op) (exeval s c allVals e)

        Binary op e f ->
            -- the error is displayed only for the 1st eval even if both fail
            B.eval op (exeval s c allVals e) (exeval s c allVals f)


step : Shared.Model -> HardLink -> Uuid -> List Uuid
step s hl uuid =
    {- For each hardlink encountered in the deeplink,
       return the list of entity uuids corresponding to the target of the hardlink
    -}
    case hl of
        ResourceLink HardLink.ResourceGroup ->
            Group.groupsOf s.state.grouped uuid

        ResourceLink HardLink.ResourceType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        EventLink HardLink.EventProvider ->
            Agent.providerOf s.state.events s.state.agents uuid

        EventLink HardLink.EventReceiver ->
            Agent.receiverOf s.state.events s.state.agents uuid

        EventLink HardLink.EventInflow ->
            Flow.flowOf s.state.events s.state.resources s.state.resourceTypes uuid

        EventLink HardLink.EventOutflow ->
            Flow.flowOf s.state.events s.state.resources s.state.resourceTypes uuid

        EventLink HardLink.EventGroup ->
            Group.groupsOf s.state.grouped uuid

        EventLink HardLink.EventType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        AgentLink HardLink.AgentGroup ->
            Group.groupsOf s.state.grouped uuid

        AgentLink HardLink.AgentType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        CommitmentLink HardLink.CommitmentProvider ->
            Agent.providerOf s.state.commitments s.state.agents uuid

        CommitmentLink HardLink.CommitmentReceiver ->
            Agent.receiverOf s.state.commitments s.state.agents uuid

        CommitmentLink HardLink.CommitmentInflow ->
            Flow.flowOf s.state.events s.state.resources s.state.resourceTypes uuid

        CommitmentLink HardLink.CommitmentOutflow ->
            Flow.flowOf s.state.events s.state.resources s.state.resourceTypes uuid

        CommitmentLink HardLink.CommitmentGroup ->
            Group.groupsOf s.state.grouped uuid

        CommitmentLink HardLink.CommitmentType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        ContractLink HardLink.ContractGroup ->
            Group.groupsOf s.state.grouped uuid

        ContractLink HardLink.ContractType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        ProcessLink HardLink.ProcessGroup ->
            Group.groupsOf s.state.grouped uuid

        ProcessLink HardLink.ProcessType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        GroupLink HardLink.ParentGroup ->
            Maybe.withDefault [] <| Maybe.map List.singleton (parentOf s.state.groups uuid)

        GroupLink HardLink.GroupGroup ->
            Group.groupsOf s.state.grouped uuid

        GroupLink HardLink.GroupType ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        ResourceTypeLink HardLink.ResourceTypeGroup ->
            Group.groupsOf s.state.grouped uuid

        ResourceTypeLink HardLink.ResourceTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        EventTypeLink HardLink.EventTypeReceiver ->
            Agent.receiversOf s.state.types s.state.eventTypes s.state.agents uuid

        EventTypeLink HardLink.EventTypeProvider ->
            Agent.providersOf s.state.types s.state.eventTypes s.state.agents uuid

        EventTypeLink HardLink.EventTypeGroup ->
            Group.groupsOf s.state.grouped uuid

        EventTypeLink HardLink.EventTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        AgentTypeLink HardLink.AgentTypeGroup ->
            Group.groupsOf s.state.grouped uuid

        AgentTypeLink HardLink.AgentTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        CommitmentTypeLink HardLink.CommitmentTypeReceiver ->
            Agent.receiversOf s.state.types s.state.commitmentTypes s.state.agents uuid

        CommitmentTypeLink HardLink.CommitmentTypeProvider ->
            Agent.providersOf s.state.types s.state.commitmentTypes s.state.agents uuid

        CommitmentTypeLink HardLink.CommitmentTypeGroup ->
            Group.groupsOf s.state.grouped uuid

        CommitmentTypeLink HardLink.CommitmentTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        ContractTypeLink HardLink.ContractTypeGroup ->
            Group.groupsOf s.state.grouped uuid

        ContractTypeLink HardLink.ContractTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        ProcessTypeLink HardLink.ProcessTypeGroup ->
            Group.groupsOf s.state.grouped uuid

        ProcessTypeLink HardLink.ProcessTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        GroupTypeLink HardLink.GroupTypeGroup ->
            Group.groupsOf s.state.grouped uuid

        GroupTypeLink HardLink.GroupTypeParent ->
            Maybe.withDefault [] <| Maybe.map List.singleton (typeOf s.state.types uuid)

        _ ->
            -- FIXME missing EventTypeFlow and CommitmentTypeFlow
            []


dleval : Shared.Model -> DeepLink -> List Uuid -> List Value
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
                    currentlist |> List.filter (\u -> toType scope |> Maybe.map (\t -> containsScope s.state.types (IsItem t u) scope) |> Maybe.withDefault False)
            in
            List.map (\g -> s.state.values |> Dict.filter (\_ v -> v.for == g) |> Dict.values) groups |> List.concat
