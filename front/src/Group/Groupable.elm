module Group.Groupable exposing (Groupable)

import Agent.Agent as Agent exposing (Agent)
import AgentType.AgentType as AgentType exposing (AgentType)
import Commitment.Commitment as Commitment exposing (Commitment)
import CommitmentType.CommitmentType as CommitmentType exposing (CommitmentType)
import Contract.Contract as Contract exposing (Contract)
import ContractType.ContractType as ContractType exposing (ContractType)
import Event.Event as Event exposing (Event)
import EventType.EventType as EventType exposing (EventType)
import Group.Group as Group exposing (Group)
import GroupType.GroupType as GroupType exposing (GroupType)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Process.Process as Process exposing (Process)
import ProcessType.ProcessType as ProcessType exposing (ProcessType)
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)


type Groupable
    = R Resource
    | E Event
    | A Agent
    | Cm Commitment
    | Cn Contract
    | P Process
    | G Group
    | RT ResourceType
    | ET EventType
    | AT AgentType
    | CmT CommitmentType
    | CnT ContractType
    | PT ProcessType
    | GT GroupType
