module REA exposing (..)

import Prng.Uuid


-- ENTITY --

type Entity =
    PROCESS Process
    | AGENT Agent
    | CONTRACTTYPE ContractType
    | CONTRACT Contract
--    | PROCESSTYPE ProcessType
--    | RESOURCETYPE ResourceType
--    | RESOURCE Resource
--    | EVENTTYPE EventType
    | EVENT Event
--    | AGENTTYPE AgentType
    | COMMITMENTTYPE CommitmentType
--    | COMMITMENT Commitment
    

-- PROCESS --

-- process type is the rea pattern
type ProcessType =
    ProcessType
    {}


-- a process is the wrapper around other entities
-- Processes are independant and unrelated one another
-- if a payment is done for 2 processes, then it's the same process
type alias Process =
        { uuid: Prng.Uuid.Uuid
        , name: String
        , contracts: List Contract
        , commitments: List Commitment
        , events: List Event
        }


-- RESSOURCE --

type ResourceType =
    ResourceType
        { name: String
        , rtype: Maybe ResourceType
        }

type alias Resource =
    { name: String
    , uuid: Prng.Uuid.Uuid
    , rtype: ResourceType
    }


-- EVENT --

type EventType =
    EventType
    { name: String
    , uuid: Prng.Uuid.Uuid
    , etype: Maybe EventType
     }


type alias Event =
    { name: String
    , uuid: Prng.Uuid.Uuid
    , etype: EventType
--    , qty: Float
--    , rtype: ResourceType
--    , provider: Agent
--    , receiver: Agent
    }


-- AGENT --

type AgentType =
    AgentType
    { name: String
    , uuid: Prng.Uuid.Uuid
    , atype: Maybe AgentType
    }


type alias Agent =
    { name: String
    , uuid: Prng.Uuid.Uuid
    , atype: AgentType
     }


-- COMMITMENT --

type CommitmentType =
    CommitmentType
        { name: String
        , uuid: Prng.Uuid.Uuid
        , ctype: Maybe CommitmentType
        }


type alias Commitment =
        { name: String
        , uuid: Prng.Uuid.Uuid
--        , ctype: CommitmentType
--        , qty: Float
--        , rtype: ResourceType
--        , provider: Agent
--        , receiver: Agent
        }


-- CONTRACT --

type ContractType =
    ContractType
    { name: String
    , ctype: Maybe ContractType
    }


type alias Contract =
    { name: String
    , ctype: ContractType
--    , parties: List Agent
--    , clauses: 
--    , terms: 
    }




