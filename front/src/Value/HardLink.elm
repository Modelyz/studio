module Value.HardLink exposing (..)


type
    HardLink
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


hardlinkToString : HardLink -> String
hardlinkToString x =
    case x of
        ResourceLink y ->
            rlToString y

        EventLink y ->
            elToString y

        AgentLink y ->
            alToString y

        CommitmentLink y ->
            cmlToString y

        ContractLink y ->
            cnlToString y

        ProcessLink y ->
            plToString y

        GroupLink y ->
            glToString y

        ResourceTypeLink y ->
            rtlToString y

        EventTypeLink y ->
            etlToString y

        AgentTypeLink y ->
            atlToString y

        CommitmentTypeLink y ->
            cmtlToString y

        ContractTypeLink y ->
            cntlToString y

        ProcessTypeLink y ->
            ptlToString y

        GroupTypeLink y ->
            gtlToString y


type
    ResourceLink
    -- Groups of type Uuid
    = ResourceGroup
      -- ResourceTypes of type Uuid
    | ResourceType


rlToString : ResourceLink -> String
rlToString x =
    case x of
        ResourceGroup ->
            "Resource Group"

        ResourceType ->
            "Resource Type"


allRL : List HardLink
allRL =
    [ ResourceLink ResourceGroup
    , ResourceLink ResourceType
    ]



--rlEval : Dict String Value -> ResourceLink -> Result String Value
--rlEval allVals link = case link of
--    ResourceGroup uuid l ->
--    ResourceType uuid l ->


type EventLink
    = EventProvider
    | EventReceiver
    | EventInflow
    | EventOutflow
    | EventGroup
    | EventType


allEL : List HardLink
allEL =
    [ EventLink EventProvider, EventLink EventReceiver, EventLink EventInflow, EventLink EventOutflow, EventLink EventGroup, EventLink EventType ]


elToString : EventLink -> String
elToString x =
    case x of
        EventProvider ->
            "Event Provider"

        EventReceiver ->
            "Event Receiver"

        EventInflow ->
            "Event Incoming resource"

        EventOutflow ->
            "Event Outgoing resource"

        EventGroup ->
            "Event Group"

        EventType ->
            "Event Type"


type AgentLink
    = AgentGroup
    | AgentType


allAL : List HardLink
allAL =
    [ AgentLink AgentGroup, AgentLink AgentType ]


alToString : AgentLink -> String
alToString x =
    case x of
        AgentGroup ->
            "Agent Group"

        AgentType ->
            "Agent Type"


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


cmlToString : CommitmentLink -> String
cmlToString x =
    case x of
        CommitmentProvider ->
            "Commitment Provider"

        CommitmentReceiver ->
            "Commitment Receiver"

        CommitmentInflow ->
            "Commitment Incoming Resource"

        CommitmentOutflow ->
            "Commitment Outgoing Resource"

        CommitmentGroup ->
            "Commitment Group"

        CommitmentType ->
            "Commitment Type"


type ContractLink
    = ContractGroup
    | ContractType


allCnL : List HardLink
allCnL =
    [ ContractLink ContractGroup, ContractLink ContractType ]


cnlToString : ContractLink -> String
cnlToString x =
    case x of
        ContractGroup ->
            "Contract Group"

        ContractType ->
            "Contract Type"


type ProcessLink
    = ProcessGroup
    | ProcessType


allPL : List HardLink
allPL =
    [ ProcessLink ProcessGroup, ProcessLink ProcessType ]


plToString : ProcessLink -> String
plToString x =
    case x of
        ProcessGroup ->
            "Process Group"

        ProcessType ->
            "Process Type"


type GroupLink
    = GroupGroup
    | GroupType


allGL : List HardLink
allGL =
    [ GroupLink GroupGroup, GroupLink GroupType ]


glToString : GroupLink -> String
glToString x =
    case x of
        GroupGroup ->
            "Group Group"

        GroupType ->
            "Group Type"


type ResourceTypeLink
    = ResourceTypeGroup
    | ResourceTypeParent


allRTL : List HardLink
allRTL =
    [ ResourceTypeLink ResourceTypeGroup, ResourceTypeLink ResourceTypeParent ]


rtlToString : ResourceTypeLink -> String
rtlToString x =
    case x of
        ResourceTypeGroup ->
            "ResourceType Group"

        ResourceTypeParent ->
            "ResourceType Parent"


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


etlToString : EventTypeLink -> String
etlToString x =
    case x of
        EventTypeProvider ->
            "Event Type Provider"

        EventTypeReceiver ->
            "Event Type Receiver"

        EventTypeInflow ->
            "Event Type Incoming Resource"

        EventTypeOutflow ->
            "Event Type Outgoing Resource"

        EventTypeGroup ->
            "Event Type Group"

        EventTypeParent ->
            "Event Type Parent Type"


type AgentTypeLink
    = AgentTypeGroup
    | AgentTypeParent


allATL : List HardLink
allATL =
    [ AgentTypeLink AgentTypeGroup, AgentTypeLink AgentTypeParent ]


atlToString : AgentTypeLink -> String
atlToString x =
    case x of
        AgentTypeGroup ->
            "Agent Type Group"

        AgentTypeParent ->
            "Agent Type Type"


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


cmtlToString : CommitmentTypeLink -> String
cmtlToString x =
    case x of
        CommitmentTypeProvider ->
            "Commitment Type Provider Agent"

        CommitmentTypeReceiver ->
            "Commitment Type Receiver Agent"

        CommitmentTypeInflow ->
            "Commitment Type Incoming Resource"

        CommitmentTypeOutflow ->
            "Commitment Type Outgoing Resource"

        CommitmentTypeGroup ->
            "Commitment Type Group"

        CommitmentTypeParent ->
            "Commitment Type Parent Type"


type ContractTypeLink
    = ContractTypeGroup
    | ContractTypeParent


allCnTL : List HardLink
allCnTL =
    [ ContractTypeLink ContractTypeGroup, ContractTypeLink ContractTypeParent ]


cntlToString : ContractTypeLink -> String
cntlToString x =
    case x of
        ContractTypeGroup ->
            "Contract Type Group"

        ContractTypeParent ->
            "Contract Type Type"


type ProcessTypeLink
    = ProcessTypeGroup
    | ProcessTypeParent


allPTL : List HardLink
allPTL =
    [ ProcessTypeLink ProcessTypeGroup, ProcessTypeLink ProcessTypeParent ]


ptlToString : ProcessTypeLink -> String
ptlToString x =
    case x of
        ProcessTypeGroup ->
            "Process Type Group"

        ProcessTypeParent ->
            "Process Type Parent Type"


type GroupTypeLink
    = GroupTypeGroup
    | GroupTypeParent


allGTL : List HardLink
allGTL =
    [ GroupTypeLink GroupTypeGroup, GroupTypeLink GroupTypeParent ]


gtlToString : GroupTypeLink -> String
gtlToString x =
    case x of
        GroupTypeGroup ->
            "Group Type Group"

        GroupTypeParent ->
            "Group Type Parent Type"


hlToChoice : HardLink -> List HardLink
hlToChoice hardlink =
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

        EventInflow ->
            allRL

        EventOutflow ->
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
