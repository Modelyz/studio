module REA exposing (..)

import Prng.Uuid

type Entity =
    Resource
    | ResourceType
    | Event
    | EventType
    | Agent
    | AgentType
    | Commitment
    | CommitmentType
    | Contract
    | ContractType
    | Process
    | ProcessType


-- PROCESS --

-- process type is the rea pattern
type alias ProcessType =
    {}


-- a process is a specific occurence of a process type
type Process =
    Process
        { uuid: Prng.Uuid.Uuid
        , name: String
        , contract: Contract
        , commitments: List Commitment
        , events: List Event
        }


-- RESSOURCE --

type ResourceType =
    ResourceType
        { name: String
        , rtype: Maybe ResourceType
        }

type Resource =
    Resource
    { name: String
    , rtype: ResourceType
    }


-- EVENT --

type EventType =
    EventType
        { name: String
        , etype: Maybe EventType
         }


type Event =
    Event
        { name: String
        , etype: EventType
        , qty: Float
        , rtype: ResourceType
        , provider: Agent
        , receiver: Agent }


-- AGENT --

type AgentType =
    AgentType
        { name: String
        , atype: Maybe AgentType
         }


type Agent =
    Agent
        { name: String
        , atype: AgentType
         }


-- COMMITMENT --


type CommitmentType =
    CommitmentType
        { name: String
        , ctype: Maybe CommitmentType
         }


type Commitment =
    Commitment
        { name: String
        , ctype: CommitmentType
        , qty: Float
        , rtype: ResourceType
        , provider: Agent
        , receiver: Agent }


-- CONTRACT --

type ContractType =
    ContractType
    { name: String
    , ctype: Maybe ContractType
    }


type Contract =
    Contract
    { name: String
    , ctype: ContractType
    , parties: List Agent
--    , clauses: 
--    , terms: 
    }




