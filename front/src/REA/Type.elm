module REA.EntityType exposing (EntityType(..), encode, getNames, toPluralString, toString, toTypeString)

import DictSet as Set exposing (DictSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import REA.AgentType as AT exposing (AgentType)
import REA.CommitmentType as CMT exposing (CommitmentType)
import REA.ContractType as CNT exposing (ContractType)
import REA.Entity exposing (Entity(..))
import REA.EventType as ET exposing (EventType)
import REA.ProcessType as PT exposing (ProcessType)
import REA.ResourceType as RT exposing (ResourceType)
import REA.Type exposing (Type)

type alias Type =
    { name : String
    , parent : Maybe String}

type EntityType
    = ResourceType Type
    | EventType Type
    | AgentType Type
    | CommitmentType Type
    | ContractType Type
    | ProcessType Type


toTypeString : EntityType -> String
toTypeString et =
    case et of
        ProcessType _ ->
            "ProcessType"

        ResourceType _ ->
            "ResourceType"

        EventType _ ->
            "EventType"

        AgentType _ ->
            "AgentType"

        CommitmentType _ ->
            "CommitmentType"

        ContractType _ ->
            "ContractType"


toString : EntityType -> String
toString et =
    case et of
        ResourceType x ->
            "ResourceType: " ++ x.name

        EventType x ->
            "EventType: " ++ x.name

        AgentType x ->
            "AgentType: " ++ x.name

        CommitmentType x ->
            "CommitmentType: " ++ x.name

        ContractType x ->
            "ResourceType: " ++ x.name

        ProcessType x ->
            "ProcessType: " ++ x.name


encode : EntityType -> Value
encode et =

    case et of
        ResourceType t ->
            Encode.object
            [ ("type", Encode.string "ResourceType")
            , ( "name", Json.Encode.string t.name )
            , ( "parent", Maybe.map Json.Encode.string t.parent |> Maybe.withDefault Json.Encode.null )
            ]

        EventType t ->
            [ ("type", Encode.string "EventType")
            , ( "name", Json.Encode.string t.name )
            , ( "parent", Maybe.map Json.Encode.string t.parent |> Maybe.withDefault Json.Encode.null )
            ]

        AgentType t ->
            [ ("type", Encode.string "AgentType")
            , ( "name", Json.Encode.string t.name )
            , ( "parent", Maybe.map Json.Encode.string t.parent |> Maybe.withDefault Json.Encode.null )
            ]

        CommitmentType t ->
            [ ("type", Encode.string "CommitmentType")
            , ( "name", Json.Encode.string t.name )
            , ( "parent", Maybe.map Json.Encode.string t.parent |> Maybe.withDefault Json.Encode.null )
            ]

        ContractType t ->
            [ ("type", Encode.string "ContractType")
            , ( "name", Json.Encode.string t.name )
            , ( "parent", Maybe.map Json.Encode.string t.parent |> Maybe.withDefault Json.Encode.null )
            ]

        ProcessType t ->
            [ ("type", Encode.string "ProcessType")
            , ( "name", Json.Encode.string t.name )
            , ( "parent", Maybe.map Json.Encode.string t.parent |> Maybe.withDefault Json.Encode.null )
            ]

decoder : Decoder EntityType
decoder = Decode.


getNames : DictSet String EntityType -> List String
getNames ets =
    ets
        |> Set.toList
        |> List.map toString


toPluralString : EntityType -> String
toPluralString ets =
    case ets of
        ProcessType _ ->
            "ProcessTypes"

        ResourceType _ ->
            "ResourceTypes"

        EventType _ ->
            "EventTypes"

        AgentType _ ->
            "AgentTypes"

        CommitmentType _ ->
            "CommitmentTypes"

        ContractType _ ->
            "ContractTypes"


compare : EntityType -> String
compare =
    toString
