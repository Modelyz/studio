module REA.EntityType exposing (EntityType(..), EntityTypes(..), decoder, encode, fromEntity, getNames, insert, remove, toPluralString, toString)

import DictSet as Set exposing (DictSet)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import REA.AgentType exposing (AgentType)
import REA.CommitmentType exposing (CommitmentType)
import REA.ContractType exposing (ContractType)
import REA.Entity exposing (Entity(..))
import REA.EventType exposing (EventType)
import REA.ProcessType exposing (ProcessType)
import REA.ResourceType as RT exposing (ResourceType)


type EntityType
    = ResourceType
    | EventType
    | AgentType
    | CommitmentType
    | ContractType
    | ProcessType


fromEntity : Entity -> EntityType
fromEntity e =
    -- starts to stink. Maybe define a separated type Type (such as Type Agent)
    case e of
        Resource ->
            ResourceType

        Event ->
            EventType

        Agent ->
            AgentType

        Commitment ->
            CommitmentType

        Contract ->
            ContractType

        Process ->
            ProcessType


type alias Name =
    String


type EntityTypes
    = ResourceTypes (DictSet String String)
    | EventTypes (DictSet String String)
    | AgentTypes (DictSet String String)
    | CommitmentTypes (DictSet String String)
    | ContractTypes (DictSet String String)
    | ProcessTypes (DictSet String String)


encode : EntityType -> Value
encode =
    toString >> Json.Encode.string


decoder : Decoder EntityType
decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (fromString
                >> Maybe.map Json.Decode.succeed
                >> Maybe.withDefault (Json.Decode.fail "Unknown entity")
            )


fromString : String -> Maybe EntityType
fromString s =
    case s of
        "ProcessType" ->
            Just ProcessType

        "ResourceType" ->
            Just ResourceType

        "EventType" ->
            Just EventType

        "AgentType" ->
            Just AgentType

        "CommitmentType" ->
            Just CommitmentType

        "ContractType" ->
            Just ContractType

        _ ->
            Nothing


toString : EntityType -> String
toString e =
    case e of
        ProcessType ->
            "ProcessType"

        ResourceType ->
            "ResourceType"

        EventType ->
            "EventType"

        AgentType ->
            "AgentType"

        CommitmentType ->
            "CommitmentType"

        ContractType ->
            "ContractType"


toPluralString : EntityType -> String
toPluralString e =
    case e of
        ProcessType ->
            "ProcessTypes"

        ResourceType ->
            "ResourceTypes"

        EventType ->
            "EventTypes"

        AgentType ->
            "AgentTypes"

        CommitmentType ->
            "CommitmentTypes"

        ContractType ->
            "ContractTypes"


getNames : EntityTypes -> List String
getNames ets =
    Set.toList <|
        case ets of
            ResourceTypes xs ->
                xs

            EventTypes xs ->
                xs

            AgentTypes xs ->
                xs

            CommitmentTypes xs ->
                xs

            ContractTypes xs ->
                xs

            ProcessTypes xs ->
                xs


insert : String -> EntityTypes -> EntityTypes
insert et ets =
    case ets of
        ResourceTypes xs ->
            ResourceTypes <| Set.insert et xs

        EventTypes xs ->
            EventTypes <| Set.insert et xs

        AgentTypes xs ->
            AgentTypes <| Set.insert et xs

        CommitmentTypes xs ->
            CommitmentTypes <| Set.insert et xs

        ContractTypes xs ->
            ContractTypes <| Set.insert et xs

        ProcessTypes xs ->
            ProcessTypes <| Set.insert et xs


remove : String -> EntityTypes -> EntityTypes
remove et ets =
    case ets of
        ResourceTypes xs ->
            ResourceTypes <| Set.remove et xs

        EventTypes xs ->
            EventTypes <| Set.remove et xs

        AgentTypes xs ->
            AgentTypes <| Set.remove et xs

        CommitmentTypes xs ->
            CommitmentTypes <| Set.remove et xs

        ContractTypes xs ->
            ContractTypes <| Set.remove et xs

        ProcessTypes xs ->
            ProcessTypes <| Set.remove et xs
