module REA.EntityType exposing (EntityType(..), EntityTypes(..), decoder, encode, encodeType, fromEntity, fromList, getNames, insert, remove, toPluralString, toString, typesDecoder)

import DictSet as Set exposing (DictSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
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


type alias Name =
    String


type EntityTypes
    = ResourceTypes (DictSet String String)
    | EventTypes (DictSet String String)
    | AgentTypes (DictSet String String)
    | CommitmentTypes (DictSet String String)
    | ContractTypes (DictSet String String)
    | ProcessTypes (DictSet String String)


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


encode : EntityType -> Value
encode =
    toString >> Encode.string


decoder : Decoder EntityType
decoder =
    Decode.string
        |> Decode.andThen
            (fromString
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Unknown entity")
            )


encodeType : EntityTypes -> Value
encodeType ets =
    case ets of
        ResourceTypes xs ->
            Encode.object [ ( "type", Encode.string "ResourceTypes" ), ( "names", Encode.list Encode.string (Set.toList xs) ) ]

        EventTypes xs ->
            Encode.object [ ( "type", Encode.string "AgentTypes" ), ( "names", Encode.list Encode.string (Set.toList xs) ) ]

        AgentTypes xs ->
            Encode.object [ ( "type", Encode.string "EventTypes" ), ( "names", Encode.list Encode.string (Set.toList xs) ) ]

        CommitmentTypes xs ->
            Encode.object [ ( "type", Encode.string "CommitmentTypes" ), ( "names", Encode.list Encode.string (Set.toList xs) ) ]

        ContractTypes xs ->
            Encode.object [ ( "type", Encode.string "ContractTypes" ), ( "names", Encode.list Encode.string (Set.toList xs) ) ]

        ProcessTypes xs ->
            Encode.object [ ( "type", Encode.string "ProcessTypes" ), ( "names", Encode.list Encode.string (Set.toList xs) ) ]


typesDecoder : Decoder EntityTypes
typesDecoder =
    let
        toSet : List String -> Decoder (DictSet String String)
        toSet ns =
            Decode.succeed (Set.fromList identity ns)
    in
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "ResourceTypes" ->
                        Decode.map ResourceTypes (Decode.field "names" (Decode.list Decode.string) |> Decode.andThen toSet)

                    "AgentTypes" ->
                        Decode.map ResourceTypes (Decode.field "names" (Decode.list Decode.string) |> Decode.andThen toSet)

                    "EventTypes" ->
                        Decode.map ResourceTypes (Decode.field "names" (Decode.list Decode.string) |> Decode.andThen toSet)

                    "CommitmentTypes" ->
                        Decode.map ResourceTypes (Decode.field "names" (Decode.list Decode.string) |> Decode.andThen toSet)

                    "ContractTypes" ->
                        Decode.map ResourceTypes (Decode.field "names" (Decode.list Decode.string) |> Decode.andThen toSet)

                    "ProcessTypes" ->
                        Decode.map ResourceTypes (Decode.field "names" (Decode.list Decode.string) |> Decode.andThen toSet)

                    _ ->
                        Decode.fail "unknown Entity Type"
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


fromList : Entity -> List String -> EntityTypes
fromList entity names =
    case entity of
        Resource ->
            ResourceTypes <| Set.fromList identity names

        Event ->
            EventTypes <| Set.fromList identity names

        Agent ->
            AgentTypes <| Set.fromList identity names

        Commitment ->
            CommitmentTypes <| Set.fromList identity names

        Contract ->
            ContractTypes <| Set.fromList identity names

        Process ->
            ProcessTypes <| Set.fromList identity names
