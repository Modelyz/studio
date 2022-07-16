module EntityType.EntityType exposing (EntityType(..), compare, decoder, encode, is, only, toEntityString, toName, toParent, toPluralString, toString, toType, toUrl)

import DictSet as Set exposing (DictSet)
import EntityType.Type as Type exposing (Type)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url exposing (percentEncode)
import Url.Builder exposing (absolute)


type EntityType
    = ResourceType Type
    | EventType Type
    | AgentType Type
    | CommitmentType Type
    | ContractType Type
    | ProcessType Type
    | GroupType Type


toEntityString : EntityType -> String
toEntityString =
    toString >> String.slice 0 -4


toString : EntityType -> String
toString et =
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

        GroupType _ ->
            "GroupType"


toType : EntityType -> Type
toType et =
    case et of
        ResourceType t ->
            t

        EventType t ->
            t

        AgentType t ->
            t

        CommitmentType t ->
            t

        ContractType t ->
            t

        ProcessType t ->
            t

        GroupType t ->
            t


toParent : EntityType -> Maybe String
toParent =
    toType >> .parent


toDesc : EntityType -> String
toDesc et =
    toEntityString et ++ " of type " ++ toName et


toName : EntityType -> String
toName =
    toType >> .name


encode : EntityType -> Value
encode et =
    case et of
        ResourceType t ->
            Encode.object
                [ ( "what", Encode.string "ResourceType" )
                , ( "type", Type.encode t )
                ]

        EventType t ->
            Encode.object
                [ ( "what", Encode.string "EventType" )
                , ( "type", Type.encode t )
                ]

        AgentType t ->
            Encode.object
                [ ( "what", Encode.string "AgentType" )
                , ( "type", Type.encode t )
                ]

        CommitmentType t ->
            Encode.object
                [ ( "what", Encode.string "CommitmentType" )
                , ( "type", Type.encode t )
                ]

        ContractType t ->
            Encode.object
                [ ( "what", Encode.string "ContractType" )
                , ( "type", Type.encode t )
                ]

        ProcessType t ->
            Encode.object
                [ ( "what", Encode.string "ProcessType" )
                , ( "type", Type.encode t )
                ]

        GroupType t ->
            Encode.object
                [ ( "what", Encode.string "GroupType" )
                , ( "type", Type.encode t )
                ]


decoder : Decoder EntityType
decoder =
    Decode.field "what" Decode.string
        |> Decode.andThen
            (\t ->
                Decode.field "type"
                    (case t of
                        "ResourceType" ->
                            Decode.map ResourceType Type.decode

                        "EventType" ->
                            Decode.map EventType Type.decode

                        "AgentType" ->
                            Decode.map AgentType Type.decode

                        "CommitmentType" ->
                            Decode.map CommitmentType Type.decode

                        "ContractType" ->
                            Decode.map ContractType Type.decode

                        "ProcessType" ->
                            Decode.map ProcessType Type.decode

                        "GroupType" ->
                            Decode.map GroupType Type.decode

                        _ ->
                            Decode.fail "Unknown entity type"
                    )
            )


toPluralString : EntityType -> String
toPluralString et =
    case et of
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

        GroupType _ ->
            "GroupTypes"


compare : EntityType -> String
compare et =
    toString et ++ " " ++ toName et


is : (Type -> EntityType) -> EntityType -> Bool
is constructor entityType =
    case entityType of
        ProcessType t ->
            constructor t == entityType

        EventType t ->
            constructor t == entityType

        AgentType t ->
            constructor t == entityType

        CommitmentType t ->
            constructor t == entityType

        ContractType t ->
            constructor t == entityType

        GroupType t ->
            constructor t == entityType

        ResourceType t ->
            constructor t == entityType


only : (Type -> EntityType) -> DictSet String EntityType -> DictSet String EntityType
only t ets =
    Set.filter (\et -> is t et) ets


select : (Type -> EntityType) -> DictSet String EntityType -> Maybe EntityType
select typename =
    only typename >> Set.toList >> List.head


toUrl : EntityType -> String
toUrl et =
    absolute
        [ case et of
            ProcessType _ ->
                "process-type"

            ResourceType _ ->
                "resource-type"

            EventType _ ->
                "event-type"

            AgentType _ ->
                "agent-type"

            CommitmentType _ ->
                "commitment-type"

            ContractType _ ->
                "contract-type"

            GroupType _ ->
                "group-type"
        , percentEncode <| toName et
        ]
        []
