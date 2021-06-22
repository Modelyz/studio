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
--    | EVENT Event
--    | AGENTTYPE AgentType
--    | COMMITMENTTYPE CommitmentType
--    | COMMITMENT Commitment
    

-- PROCESS --

-- process type is the rea pattern
type ProcessType =
    ProcessType
    {}


-- a process is a specific occurence of a process type
type alias Process =
        { uuid: Prng.Uuid.Uuid
        , name: String
        , contract: Contract
--        , commitments: List Commitment
--        , events: List Event
        }


-- RESSOURCE --

type ResourceType =
    ResourceType
        { name: String
        , rtype: Maybe ResourceType
        }

type alias Resource =
    { name: String
    , rtype: ResourceType
    }
-- EVENT --

type EventType =
    EventType
    { name: String
    , etype: Maybe EventType
     }


type alias Event =
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


type alias Agent =
        { name: String
        , atype: AgentType
         }


-- COMMITMENT --

type CommitmentType =
    CommitmentType
        { name: String
        , ctype: Maybe CommitmentType
         }


type alias Commitment =
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


type alias Contract =
    { name: String
    , ctype: ContractType
    , parties: List Agent
--    , clauses: 
--    , terms: 
    }




