module Value.DeepLink exposing (DeepLink(..), addTail, isComplete, terminate, toChoice, toDisplay, toScope, toString)

import Dict
import Hierarchy.Type as HType
import Scope.Scope as Scope exposing (Scope(..))
import Type exposing (Type)
import Typed.Type as TType
import Value.HardLink as HL exposing (HardLink)


type DeepLink
    = Null
    | Link HardLink DeepLink
      -- the endpoint corresponds to a ValueType
      -- the EndPoint scope is the restriction given by the last hardlink destination
    | EndPoint Scope String


toString : DeepLink -> String
toString v =
    case v of
        Link hl dl ->
            "Link"

        EndPoint _ name ->
            -- FIXME
            "EndPoint : " ++ name

        Null ->
            -- FIXME
            "Null"


addTail : HardLink -> DeepLink -> DeepLink
addTail hl dl =
    case dl of
        Null ->
            Link hl dl

        Link hl2 dl2 ->
            Link hl2 (addTail hl dl2)

        EndPoint scope name ->
            Link hl (EndPoint scope name)


isComplete : DeepLink -> Bool
isComplete deeplink =
    case deeplink of
        Null ->
            False

        Link hl dl ->
            isComplete dl

        EndPoint scope name ->
            True


terminate : Scope -> String -> DeepLink -> DeepLink
terminate scope name dl =
    case dl of
        Null ->
            EndPoint scope name

        Link hl dl2 ->
            Link hl (terminate scope name dl2)

        EndPoint _ _ ->
            EndPoint scope name


toDisplay : DeepLink -> String
toDisplay deeplink =
    case deeplink of
        Null ->
            "Empty"

        Link hl dl ->
            HL.toString hl ++ " → " ++ toDisplay dl

        EndPoint scope name ->
            -- TODO display without uuid
            -- name ++ " (" Scope.View.toDisplay allT allG configs scope ++ ")"
            name ++ " (" ++ Scope.toString scope ++ ")"


toScope : Scope -> DeepLink -> Scope
toScope scope deeplink =
    -- TODO pass the restriction scope on the hardlink to narrow the choice of values
    case deeplink of
        Null ->
            scope

        Link hl dl ->
            toScope (hlToScope hl) dl

        EndPoint vtscope name ->
            scope


toChoice : Scope -> List HardLink
toChoice scope =
    case scope of
        HasUserType t ht uuid ->
            case t of
                Type.TType TType.Resource ->
                    HL.allRL

                Type.TType TType.Event ->
                    HL.allEL

                Type.TType TType.Agent ->
                    HL.allAL

                Type.TType TType.Commitment ->
                    HL.allCmL

                Type.TType TType.Contract ->
                    HL.allCnL

                Type.TType TType.Process ->
                    HL.allPL

                Type.TType TType.Group ->
                    HL.allGL

                Type.HType HType.ResourceType ->
                    HL.allRL

                Type.HType HType.EventType ->
                    HL.allEL

                Type.HType HType.AgentType ->
                    HL.allAL

                Type.HType HType.CommitmentType ->
                    HL.allCmL

                Type.HType HType.ContractType ->
                    HL.allCnL

                Type.HType HType.ProcessType ->
                    HL.allPL

                Type.HType HType.GroupType ->
                    HL.allGL

        _ ->
            []


hlToScope : HardLink -> Scope
hlToScope x =
    case x of
        HL.ResourceLink y ->
            rlToScope y

        HL.EventLink y ->
            elToScope y

        HL.AgentLink y ->
            alToScope y

        HL.CommitmentLink y ->
            cmlToScope y

        HL.ContractLink y ->
            cnlToScope y

        HL.ProcessLink y ->
            plToScope y

        HL.GroupLink y ->
            glToScope y

        HL.ResourceTypeLink y ->
            rtlToScope y

        HL.EventTypeLink y ->
            etlToScope y

        HL.AgentTypeLink y ->
            atlToScope y

        HL.CommitmentTypeLink y ->
            cmtlToScope y

        HL.ContractTypeLink y ->
            cntlToScope y

        HL.ProcessTypeLink y ->
            ptlToScope y

        HL.GroupTypeLink y ->
            gtlToScope y


rlToScope : HL.ResourceLink -> Scope
rlToScope x =
    case x of
        HL.ResourceGroup ->
            HasType (Type.TType TType.Group)

        HL.ResourceType ->
            HasType (Type.HType HType.ResourceType)


elToScope : HL.EventLink -> Scope
elToScope x =
    case x of
        HL.EventProvider ->
            HasType (Type.TType TType.Agent)

        HL.EventReceiver ->
            HasType (Type.TType TType.Agent)

        HL.EventInflow ->
            HasType (Type.TType TType.Resource)

        HL.EventOutflow ->
            HasType (Type.TType TType.Resource)

        HL.EventGroup ->
            HasType (Type.TType TType.Group)

        HL.EventType ->
            HasType (Type.HType HType.EventType)


alToScope : HL.AgentLink -> Scope
alToScope x =
    case x of
        HL.AgentGroup ->
            HasType (Type.TType TType.Group)

        HL.AgentType ->
            HasType (Type.HType HType.AgentType)


cmlToScope : HL.CommitmentLink -> Scope
cmlToScope x =
    case x of
        HL.CommitmentProvider ->
            HasType (Type.TType TType.Agent)

        HL.CommitmentReceiver ->
            HasType (Type.TType TType.Agent)

        HL.CommitmentInflow ->
            HasType (Type.TType TType.Resource)

        HL.CommitmentOutflow ->
            HasType (Type.TType TType.Resource)

        HL.CommitmentGroup ->
            HasType (Type.TType TType.Group)

        HL.CommitmentType ->
            HasType (Type.HType HType.CommitmentType)


cnlToScope : HL.ContractLink -> Scope
cnlToScope x =
    case x of
        HL.ContractGroup ->
            HasType (Type.TType TType.Group)

        HL.ContractType ->
            HasType (Type.HType HType.ContractType)


plToScope : HL.ProcessLink -> Scope
plToScope x =
    case x of
        HL.ProcessGroup ->
            HasType (Type.TType TType.Group)

        HL.ProcessType ->
            HasType (Type.HType HType.ProcessType)


glToScope : HL.GroupLink -> Scope
glToScope x =
    case x of
        HL.GroupGroup ->
            HasType (Type.TType TType.Group)

        HL.GroupType ->
            HasType (Type.HType HType.GroupType)


rtlToScope : HL.ResourceTypeLink -> Scope
rtlToScope x =
    case x of
        HL.ResourceTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.ResourceTypeParent ->
            HasType (Type.HType HType.ResourceType)


etlToScope : HL.EventTypeLink -> Scope
etlToScope x =
    case x of
        HL.EventTypeProvider ->
            HasType (Type.TType TType.Agent)

        HL.EventTypeReceiver ->
            HasType (Type.TType TType.Agent)

        HL.EventTypeInflow ->
            -- TODO an inflow can also defined by be a RT
            HasType (Type.TType TType.Resource)

        HL.EventTypeOutflow ->
            HasType (Type.TType TType.Resource)

        HL.EventTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.EventTypeParent ->
            HasType (Type.HType HType.EventType)


atlToScope : HL.AgentTypeLink -> Scope
atlToScope x =
    case x of
        HL.AgentTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.AgentTypeParent ->
            HasType (Type.HType HType.AgentType)


cmtlToScope : HL.CommitmentTypeLink -> Scope
cmtlToScope x =
    case x of
        HL.CommitmentTypeProvider ->
            HasType (Type.TType TType.Agent)

        HL.CommitmentTypeReceiver ->
            HasType (Type.TType TType.Agent)

        HL.CommitmentTypeInflow ->
            HasType (Type.TType TType.Resource)

        HL.CommitmentTypeOutflow ->
            HasType (Type.TType TType.Resource)

        HL.CommitmentTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.CommitmentTypeParent ->
            HasType (Type.HType HType.CommitmentType)


cntlToScope : HL.ContractTypeLink -> Scope
cntlToScope x =
    case x of
        HL.ContractTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.ContractTypeParent ->
            HasType (Type.HType HType.ContractType)


ptlToScope : HL.ProcessTypeLink -> Scope
ptlToScope x =
    case x of
        HL.ProcessTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.ProcessTypeParent ->
            HasType (Type.HType HType.ProcessType)


gtlToScope : HL.GroupTypeLink -> Scope
gtlToScope x =
    case x of
        HL.GroupTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.GroupTypeParent ->
            HasType (Type.HType HType.GroupType)
