module Expression.HardLink exposing (AgentLink(..), AgentTypeLink(..), CommitmentLink(..), CommitmentTypeLink(..), ContractLink(..), ContractTypeLink(..), EventLink(..), EventTypeLink(..), GroupLink(..), GroupTypeLink(..), HardLink(..), ProcessLink(..), ProcessTypeLink(..), ResourceLink(..), ResourceTypeLink(..), chooseFromScope, decoder, encode, toChoice, toScope, toString)

import Hierarchy.Type as HType
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Scope exposing (Scope(..))
import Type exposing (Type)
import Typed.Type as TType


type
    HardLink
    -- a hardlink is a field of an entity
    -- automatically chosen depending on the context ie the selected scope
    = ResourceLink ResourceLink
    | EventLink EventLink
    | AgentLink AgentLink
    | CommitmentLink CommitmentLink
    | ContractLink ContractLink
    | ProcessLink ProcessLink
    | GroupLink GroupLink
    | ResourceTypeLink ResourceTypeLink
    | EventTypeLink EventTypeLink
    | AgentTypeLink AgentTypeLink
    | CommitmentTypeLink CommitmentTypeLink
    | ContractTypeLink ContractTypeLink
    | ProcessTypeLink ProcessTypeLink
    | GroupTypeLink GroupTypeLink


toString : HardLink -> String
toString hardlink =
    case hardlink of
        ResourceLink ResourceGroup ->
            "Resource Group"

        ResourceLink ResourceType ->
            "Resource Type"

        EventLink EventProvider ->
            "Event Provider"

        EventLink EventReceiver ->
            "Event Receiver"

        EventLink EventResource ->
            "Event Resource"

        EventLink EventGroup ->
            "Event Group"

        EventLink EventType ->
            "Event Type"

        AgentLink AgentGroup ->
            "Agent Group"

        AgentLink AgentType ->
            "Agent Type"

        CommitmentLink CommitmentProvider ->
            "Commitment Provider"

        CommitmentLink CommitmentReceiver ->
            "Commitment Receiver"

        CommitmentLink CommitmentInflow ->
            "Commitment Incoming Resource"

        CommitmentLink CommitmentOutflow ->
            "Commitment Outgoing Resource"

        CommitmentLink CommitmentGroup ->
            "Commitment Group"

        CommitmentLink CommitmentType ->
            "Commitment Type"

        ContractLink ContractGroup ->
            "Contract Group"

        ContractLink ContractType ->
            "Contract Type"

        ProcessLink ProcessGroup ->
            "Process Group"

        ProcessLink ProcessType ->
            "Process Type"

        GroupLink GroupGroup ->
            "Group Group"

        GroupLink GroupType ->
            "Group Type"

        GroupLink ParentGroup ->
            "Parent Group"

        ResourceTypeLink ResourceTypeGroup ->
            "ResourceType Group"

        ResourceTypeLink ResourceTypeParent ->
            "ResourceType Parent"

        EventTypeLink EventTypeProvider ->
            "Event Type Provider"

        EventTypeLink EventTypeReceiver ->
            "Event Type Receiver"

        EventTypeLink EventTypeInflow ->
            "Event Type Incoming Resource"

        EventTypeLink EventTypeOutflow ->
            "Event Type Outgoing Resource"

        EventTypeLink EventTypeGroup ->
            "Event Type Group"

        EventTypeLink EventTypeParent ->
            "Event Type Parent Type"

        AgentTypeLink AgentTypeGroup ->
            "Agent Type Group"

        AgentTypeLink AgentTypeParent ->
            "Agent Type Type"

        CommitmentTypeLink CommitmentTypeProvider ->
            "Commitment Type Provider Agent"

        CommitmentTypeLink CommitmentTypeReceiver ->
            "Commitment Type Receiver Agent"

        CommitmentTypeLink CommitmentTypeInflow ->
            "Commitment Type Incoming Resource"

        CommitmentTypeLink CommitmentTypeOutflow ->
            "Commitment Type Outgoing Resource"

        CommitmentTypeLink CommitmentTypeGroup ->
            "Commitment Type Group"

        CommitmentTypeLink CommitmentTypeParent ->
            "Commitment Type Parent Type"

        ContractTypeLink ContractTypeGroup ->
            "Contract Type Group"

        ContractTypeLink ContractTypeParent ->
            "Contract Type Type"

        ProcessTypeLink ProcessTypeGroup ->
            "Process Type Group"

        ProcessTypeLink ProcessTypeParent ->
            "Process Type Parent Type"

        GroupTypeLink GroupTypeGroup ->
            "Group Type Group"

        GroupTypeLink GroupTypeParent ->
            "Group Type Parent Type"


encode : HardLink -> Encode.Value
encode hl =
    Encode.string <| toString hl


decoder : Decoder HardLink
decoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Resource Group" ->
                        Decode.succeed <| ResourceLink ResourceGroup

                    "Resource Type" ->
                        Decode.succeed <| ResourceLink ResourceType

                    "Event Provider" ->
                        Decode.succeed <| EventLink EventProvider

                    "Event Receiver" ->
                        Decode.succeed <| EventLink EventReceiver

                    "Event Resource" ->
                        Decode.succeed <| EventLink EventResource

                    "Event Group" ->
                        Decode.succeed <| EventLink EventGroup

                    "Event Type" ->
                        Decode.succeed <| EventLink EventType

                    "Agent Group" ->
                        Decode.succeed <| AgentLink AgentGroup

                    "Agent Type" ->
                        Decode.succeed <| AgentLink AgentType

                    "Commitment Provider" ->
                        Decode.succeed <| CommitmentLink CommitmentProvider

                    "Commitment Receiver" ->
                        Decode.succeed <| CommitmentLink CommitmentReceiver

                    "Commitment Incoming Resource" ->
                        Decode.succeed <| CommitmentLink CommitmentInflow

                    "Commitment Outgoing Resource" ->
                        Decode.succeed <| CommitmentLink CommitmentOutflow

                    "Commitment Group" ->
                        Decode.succeed <| CommitmentLink CommitmentGroup

                    "Commitment Type" ->
                        Decode.succeed <| CommitmentLink CommitmentType

                    "Contract Group" ->
                        Decode.succeed <| ContractLink ContractGroup

                    "Contract Type" ->
                        Decode.succeed <| ContractLink ContractType

                    "Process Group" ->
                        Decode.succeed <| ProcessLink ProcessGroup

                    "Process Type" ->
                        Decode.succeed <| ProcessLink ProcessType

                    "Group Group" ->
                        Decode.succeed <| GroupLink GroupGroup

                    "Group Type" ->
                        Decode.succeed <| GroupLink GroupType

                    "Parent Group" ->
                        Decode.succeed <| GroupLink ParentGroup

                    "ResourceType Group" ->
                        Decode.succeed <| ResourceTypeLink ResourceTypeGroup

                    "ResourceType Parent" ->
                        Decode.succeed <| ResourceTypeLink ResourceTypeParent

                    "Event Type Provider" ->
                        Decode.succeed <| EventTypeLink EventTypeProvider

                    "Event Type Receiver" ->
                        Decode.succeed <| EventTypeLink EventTypeReceiver

                    "Event Type Incoming Resource" ->
                        Decode.succeed <| EventTypeLink EventTypeInflow

                    "Event Type Outgoing Resource" ->
                        Decode.succeed <| EventTypeLink EventTypeOutflow

                    "Event Type Group" ->
                        Decode.succeed <| EventTypeLink EventTypeGroup

                    "Event Type Parent Type" ->
                        Decode.succeed <| EventTypeLink EventTypeParent

                    "Agent Type Group" ->
                        Decode.succeed <| AgentTypeLink AgentTypeGroup

                    "Agent Type Type" ->
                        Decode.succeed <| AgentTypeLink AgentTypeParent

                    "Commitment Type Provider Agent" ->
                        Decode.succeed <| CommitmentTypeLink CommitmentTypeProvider

                    "Commitment Type Receiver Agent" ->
                        Decode.succeed <| CommitmentTypeLink CommitmentTypeReceiver

                    "Commitment Type Incoming Resource" ->
                        Decode.succeed <| CommitmentTypeLink CommitmentTypeInflow

                    "Commitment Type Outgoing Resource" ->
                        Decode.succeed <| CommitmentTypeLink CommitmentTypeOutflow

                    "Commitment Type Group" ->
                        Decode.succeed <| CommitmentTypeLink CommitmentTypeGroup

                    "Commitment Type Parent Type" ->
                        Decode.succeed <| CommitmentTypeLink CommitmentTypeParent

                    "Contract Type Group" ->
                        Decode.succeed <| ContractTypeLink ContractTypeGroup

                    "Contract Type Type" ->
                        Decode.succeed <| ContractTypeLink ContractTypeParent

                    "Process Type Group" ->
                        Decode.succeed <| ProcessTypeLink ProcessTypeGroup

                    "Process Type Parent Type" ->
                        Decode.succeed <| ProcessTypeLink ProcessTypeParent

                    "Group Type Group" ->
                        Decode.succeed <| GroupTypeLink GroupTypeGroup

                    "Group Type Parent Type" ->
                        Decode.succeed <| GroupTypeLink GroupTypeParent

                    _ ->
                        Decode.fail "Unknown hardlink"
            )


type
    ResourceLink
    -- Groups of type Uuid
    = ResourceGroup
      -- ResourceTypes of type Uuid
    | ResourceType


allRL : List HardLink
allRL =
    [ ResourceLink ResourceGroup
    , ResourceLink ResourceType
    ]


type EventLink
    = EventProvider
    | EventReceiver
    | EventResource
    | EventGroup
    | EventType


allEL : List HardLink
allEL =
    [ EventLink EventProvider, EventLink EventReceiver, EventLink EventResource, EventLink EventGroup, EventLink EventType ]


type AgentLink
    = AgentGroup
    | AgentType


allAL : List HardLink
allAL =
    [ AgentLink AgentGroup, AgentLink AgentType ]


type CommitmentLink
    = CommitmentProvider
    | CommitmentReceiver
    | CommitmentInflow
    | CommitmentOutflow
    | CommitmentGroup
    | CommitmentType


allCmL : List HardLink
allCmL =
    [ CommitmentLink CommitmentProvider
    , CommitmentLink CommitmentReceiver
    , CommitmentLink CommitmentInflow
    , CommitmentLink CommitmentOutflow
    , CommitmentLink CommitmentGroup
    , CommitmentLink CommitmentType
    ]


type ContractLink
    = ContractGroup
    | ContractType


allCnL : List HardLink
allCnL =
    [ ContractLink ContractGroup, ContractLink ContractType ]


type ProcessLink
    = ProcessGroup
    | ProcessType


allPL : List HardLink
allPL =
    [ ProcessLink ProcessGroup, ProcessLink ProcessType ]


type GroupLink
    = GroupGroup
    | GroupType
    | ParentGroup


allGL : List HardLink
allGL =
    [ GroupLink ParentGroup, GroupLink GroupGroup, GroupLink GroupType ]


type ResourceTypeLink
    = ResourceTypeGroup
    | ResourceTypeParent


allRTL : List HardLink
allRTL =
    [ ResourceTypeLink ResourceTypeGroup, ResourceTypeLink ResourceTypeParent ]


type EventTypeLink
    = EventTypeProvider
    | EventTypeReceiver
    | EventTypeInflow
    | EventTypeOutflow
    | EventTypeGroup
    | EventTypeParent


allETL : List HardLink
allETL =
    [ EventTypeLink EventTypeProvider, EventTypeLink EventTypeReceiver, EventTypeLink EventTypeInflow, EventTypeLink EventTypeOutflow, EventTypeLink EventTypeGroup, EventTypeLink EventTypeParent ]


type AgentTypeLink
    = AgentTypeGroup
    | AgentTypeParent


allATL : List HardLink
allATL =
    [ AgentTypeLink AgentTypeGroup, AgentTypeLink AgentTypeParent ]


type CommitmentTypeLink
    = CommitmentTypeProvider
    | CommitmentTypeReceiver
    | CommitmentTypeInflow
    | CommitmentTypeOutflow
    | CommitmentTypeGroup
    | CommitmentTypeParent


allCmTL : List HardLink
allCmTL =
    [ CommitmentTypeLink CommitmentTypeProvider, CommitmentTypeLink CommitmentTypeReceiver, CommitmentTypeLink CommitmentTypeInflow, CommitmentTypeLink CommitmentTypeOutflow, CommitmentTypeLink CommitmentTypeGroup, CommitmentTypeLink CommitmentTypeParent ]


type ContractTypeLink
    = ContractTypeGroup
    | ContractTypeParent


allCnTL : List HardLink
allCnTL =
    [ ContractTypeLink ContractTypeGroup, ContractTypeLink ContractTypeParent ]


type ProcessTypeLink
    = ProcessTypeGroup
    | ProcessTypeParent


allPTL : List HardLink
allPTL =
    [ ProcessTypeLink ProcessTypeGroup, ProcessTypeLink ProcessTypeParent ]


type GroupTypeLink
    = GroupTypeGroup
    | GroupTypeParent


allGTL : List HardLink
allGTL =
    [ GroupTypeLink GroupTypeGroup, GroupTypeLink GroupTypeParent ]


toChoice : HardLink -> List HardLink
toChoice hardlink =
    case hardlink of
        ResourceLink y ->
            rlToChoice y

        EventLink y ->
            elToChoice y

        AgentLink y ->
            alToChoice y

        CommitmentLink y ->
            cmlToChoice y

        ContractLink y ->
            cnlToChoice y

        ProcessLink y ->
            plToChoice y

        GroupLink y ->
            glToChoice y

        ResourceTypeLink y ->
            rtlToChoice y

        EventTypeLink y ->
            etlToChoice y

        AgentTypeLink y ->
            atlToChoice y

        CommitmentTypeLink y ->
            cmtlToChoice y

        ContractTypeLink y ->
            cntlToChoice y

        ProcessTypeLink y ->
            ptlToChoice y

        GroupTypeLink y ->
            gtlToChoice y


rlToChoice : ResourceLink -> List HardLink
rlToChoice hl =
    case hl of
        ResourceGroup ->
            allGL

        ResourceType ->
            allRTL


elToChoice : EventLink -> List HardLink
elToChoice hl =
    case hl of
        EventProvider ->
            allAL

        EventReceiver ->
            allAL

        EventResource ->
            allRL

        EventGroup ->
            allGL

        EventType ->
            allETL


alToChoice : AgentLink -> List HardLink
alToChoice hl =
    case hl of
        AgentGroup ->
            allGL

        AgentType ->
            allATL


cmlToChoice : CommitmentLink -> List HardLink
cmlToChoice hl =
    case hl of
        CommitmentProvider ->
            allAL

        CommitmentReceiver ->
            allAL

        CommitmentInflow ->
            allRL

        CommitmentOutflow ->
            allRL

        CommitmentGroup ->
            allGL

        CommitmentType ->
            allCmTL


cnlToChoice : ContractLink -> List HardLink
cnlToChoice hl =
    case hl of
        ContractGroup ->
            allGL

        ContractType ->
            allCnTL


plToChoice : ProcessLink -> List HardLink
plToChoice hl =
    case hl of
        ProcessGroup ->
            allGL

        ProcessType ->
            allPTL


glToChoice : GroupLink -> List HardLink
glToChoice hl =
    case hl of
        ParentGroup ->
            allGL

        GroupGroup ->
            allGL

        GroupType ->
            allGTL


rtlToChoice : ResourceTypeLink -> List HardLink
rtlToChoice hl =
    case hl of
        ResourceTypeGroup ->
            allGL

        ResourceTypeParent ->
            allRTL


etlToChoice : EventTypeLink -> List HardLink
etlToChoice hl =
    case hl of
        EventTypeProvider ->
            allAL

        EventTypeReceiver ->
            allAL

        EventTypeInflow ->
            allRL

        EventTypeOutflow ->
            allRL

        EventTypeGroup ->
            allGL

        EventTypeParent ->
            allETL


atlToChoice : AgentTypeLink -> List HardLink
atlToChoice hl =
    case hl of
        AgentTypeGroup ->
            allGL

        AgentTypeParent ->
            allATL


cmtlToChoice : CommitmentTypeLink -> List HardLink
cmtlToChoice hl =
    case hl of
        CommitmentTypeProvider ->
            allAL

        CommitmentTypeReceiver ->
            allAL

        CommitmentTypeInflow ->
            allRL

        CommitmentTypeOutflow ->
            allRL

        CommitmentTypeGroup ->
            allGL

        CommitmentTypeParent ->
            allCmTL


cntlToChoice : ContractTypeLink -> List HardLink
cntlToChoice hl =
    case hl of
        ContractTypeGroup ->
            allGL

        ContractTypeParent ->
            allCnTL


ptlToChoice : ProcessTypeLink -> List HardLink
ptlToChoice hl =
    case hl of
        ProcessTypeGroup ->
            allGL

        ProcessTypeParent ->
            allPTL


gtlToChoice : GroupTypeLink -> List HardLink
gtlToChoice hl =
    case hl of
        GroupTypeGroup ->
            allGL

        GroupTypeParent ->
            allGTL


toScope : HardLink -> Scope
toScope hardlink =
    case hardlink of
        ResourceLink y ->
            rlToScope y

        EventLink y ->
            elToScope y

        AgentLink y ->
            alToScope y

        CommitmentLink y ->
            cmlToScope y

        ContractLink y ->
            cnlToScope y

        ProcessLink y ->
            plToScope y

        GroupLink y ->
            glToScope y

        ResourceTypeLink y ->
            rtlToScope y

        EventTypeLink y ->
            etlToScope y

        AgentTypeLink y ->
            atlToScope y

        CommitmentTypeLink y ->
            cmtlToScope y

        ContractTypeLink y ->
            cntlToScope y

        ProcessTypeLink y ->
            ptlToScope y

        GroupTypeLink y ->
            gtlToScope y


rlToScope : ResourceLink -> Scope
rlToScope hardlink =
    case hardlink of
        ResourceGroup ->
            HasType (Type.TType TType.Group)

        ResourceType ->
            HasType (Type.HType HType.ResourceType)


elToScope : EventLink -> Scope
elToScope hardlink =
    case hardlink of
        EventProvider ->
            HasType (Type.TType TType.Agent)

        EventReceiver ->
            HasType (Type.TType TType.Agent)

        EventResource ->
            HasType (Type.TType TType.Resource)

        EventGroup ->
            HasType (Type.TType TType.Group)

        EventType ->
            HasType (Type.HType HType.EventType)


alToScope : AgentLink -> Scope
alToScope hardlink =
    case hardlink of
        AgentGroup ->
            HasType (Type.TType TType.Group)

        AgentType ->
            HasType (Type.HType HType.AgentType)


cmlToScope : CommitmentLink -> Scope
cmlToScope hardlink =
    case hardlink of
        CommitmentProvider ->
            HasType (Type.TType TType.Agent)

        CommitmentReceiver ->
            HasType (Type.TType TType.Agent)

        CommitmentInflow ->
            HasType (Type.TType TType.Resource)

        CommitmentOutflow ->
            HasType (Type.TType TType.Resource)

        CommitmentGroup ->
            HasType (Type.TType TType.Group)

        CommitmentType ->
            HasType (Type.HType HType.CommitmentType)


cnlToScope : ContractLink -> Scope
cnlToScope hardlink =
    case hardlink of
        ContractGroup ->
            HasType (Type.TType TType.Group)

        ContractType ->
            HasType (Type.HType HType.ContractType)


plToScope : ProcessLink -> Scope
plToScope hardlink =
    case hardlink of
        ProcessGroup ->
            HasType (Type.TType TType.Group)

        ProcessType ->
            HasType (Type.HType HType.ProcessType)


glToScope : GroupLink -> Scope
glToScope hardlink =
    case hardlink of
        ParentGroup ->
            HasType (Type.TType TType.Group)

        GroupGroup ->
            HasType (Type.TType TType.Group)

        GroupType ->
            HasType (Type.HType HType.GroupType)


rtlToScope : ResourceTypeLink -> Scope
rtlToScope hardlink =
    case hardlink of
        ResourceTypeGroup ->
            HasType (Type.TType TType.Group)

        ResourceTypeParent ->
            HasType (Type.HType HType.ResourceType)


etlToScope : EventTypeLink -> Scope
etlToScope hardlink =
    case hardlink of
        EventTypeProvider ->
            HasType (Type.TType TType.Agent)

        EventTypeReceiver ->
            HasType (Type.TType TType.Agent)

        EventTypeInflow ->
            -- TODO an inflow can also defined by be a RT
            HasType (Type.TType TType.Resource)

        EventTypeOutflow ->
            HasType (Type.TType TType.Resource)

        EventTypeGroup ->
            HasType (Type.TType TType.Group)

        EventTypeParent ->
            HasType (Type.HType HType.EventType)


atlToScope : AgentTypeLink -> Scope
atlToScope hardlink =
    case hardlink of
        AgentTypeGroup ->
            HasType (Type.TType TType.Group)

        AgentTypeParent ->
            HasType (Type.HType HType.AgentType)


cmtlToScope : CommitmentTypeLink -> Scope
cmtlToScope hardlink =
    case hardlink of
        CommitmentTypeProvider ->
            HasType (Type.TType TType.Agent)

        CommitmentTypeReceiver ->
            HasType (Type.TType TType.Agent)

        CommitmentTypeInflow ->
            HasType (Type.TType TType.Resource)

        CommitmentTypeOutflow ->
            HasType (Type.TType TType.Resource)

        CommitmentTypeGroup ->
            HasType (Type.TType TType.Group)

        CommitmentTypeParent ->
            HasType (Type.HType HType.CommitmentType)


cntlToScope : ContractTypeLink -> Scope
cntlToScope hardlink =
    case hardlink of
        ContractTypeGroup ->
            HasType (Type.TType TType.Group)

        ContractTypeParent ->
            HasType (Type.HType HType.ContractType)


ptlToScope : ProcessTypeLink -> Scope
ptlToScope hardlink =
    case hardlink of
        ProcessTypeGroup ->
            HasType (Type.TType TType.Group)

        ProcessTypeParent ->
            HasType (Type.HType HType.ProcessType)


gtlToScope : GroupTypeLink -> Scope
gtlToScope hardlink =
    case hardlink of
        GroupTypeGroup ->
            HasType (Type.TType TType.Group)

        GroupTypeParent ->
            HasType (Type.HType HType.GroupType)


chooseFromType : Type -> List HardLink
chooseFromType t =
    case t of
        Type.TType TType.Resource ->
            allRL

        Type.TType TType.Event ->
            allEL

        Type.TType TType.Agent ->
            allAL

        Type.TType TType.Commitment ->
            allCmL

        Type.TType TType.Contract ->
            allCnL

        Type.TType TType.Process ->
            allPL

        Type.TType TType.Group ->
            allGL

        Type.HType HType.ResourceType ->
            allRTL

        Type.HType HType.EventType ->
            allETL

        Type.HType HType.AgentType ->
            allATL

        Type.HType HType.CommitmentType ->
            allCmTL

        Type.HType HType.ContractType ->
            allCnTL

        Type.HType HType.ProcessType ->
            allPTL

        Type.HType HType.GroupType ->
            allGTL


chooseFromScope : Scope -> List HardLink
chooseFromScope scope =
    case scope of
        HasType t ->
            chooseFromType t

        HasUserType t _ ->
            chooseFromType t

        _ ->
            []
