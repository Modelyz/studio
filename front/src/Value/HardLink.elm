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


enlToString : HardLink -> String
enlToString x =
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
    | EventTypeType


allETL : List HardLink
allETL =
    [ EventTypeLink EventTypeProvider, EventTypeLink EventTypeReceiver, EventTypeLink EventTypeInflow, EventTypeLink EventTypeOutflow, EventTypeLink EventTypeGroup, EventTypeLink EventTypeType ]


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

        EventTypeType ->
            "Event Type Parent Type"


type AgentTypeLink
    = AgentTypeGroup
    | AgentTypeType


allATL : List HardLink
allATL =
    [ AgentTypeLink AgentTypeGroup, AgentTypeLink AgentTypeType ]


atlToString : AgentTypeLink -> String
atlToString x =
    case x of
        AgentTypeGroup ->
            "Agent Type Group"

        AgentTypeType ->
            "Agent Type Type"


type CommitmentTypeLink
    = CommitmentTypeProvider
    | CommitmentTypeReceiver
    | CommitmentTypeInflow
    | CommitmentTypeOutflow
    | CommitmentTypeGroup
    | CommitmentTypeType


allCmTL : List HardLink
allCmTL =
    [ CommitmentTypeLink CommitmentTypeProvider, CommitmentTypeLink CommitmentTypeReceiver, CommitmentTypeLink CommitmentTypeInflow, CommitmentTypeLink CommitmentTypeOutflow, CommitmentTypeLink CommitmentTypeGroup, CommitmentTypeLink CommitmentTypeType ]


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

        CommitmentTypeType ->
            "Commitment Type Parent Type"


type ContractTypeLink
    = ContractTypeGroup
    | ContractTypeType


allCnTL : List HardLink
allCnTL =
    [ ContractTypeLink ContractTypeGroup, ContractTypeLink ContractTypeType ]


cntlToString : ContractTypeLink -> String
cntlToString x =
    case x of
        ContractTypeGroup ->
            "Contract Type Group"

        ContractTypeType ->
            "Contract Type Type"


type ProcessTypeLink
    = ProcessTypeGroup
    | ProcessTypeType


allPTL : List HardLink
allPTL =
    [ ProcessTypeLink ProcessTypeGroup, ProcessTypeLink ProcessTypeType ]


ptlToString : ProcessTypeLink -> String
ptlToString x =
    case x of
        ProcessTypeGroup ->
            "Process Type Group"

        ProcessTypeType ->
            "Process Type Parent Type"


type GroupTypeLink
    = GroupTypeGroup
    | GroupTypeType


allGTL : List HardLink
allGTL =
    [ GroupTypeLink GroupTypeGroup, GroupTypeLink GroupTypeType ]


gtlToString : GroupTypeLink -> String
gtlToString x =
    case x of
        GroupTypeGroup ->
            "Group Type Group"

        GroupTypeType ->
            "Group Type Parent Type"
