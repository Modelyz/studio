module REA.EntityType exposing (EntityType(..), compare, decoder, encode, onlyType, toName, toParent, toPluralString, toString, toType, toUrl)

import DictSet as Set exposing (DictSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import REA.Entity exposing (Entity(..))
import REA.Type as T exposing (Type)
import Url exposing (percentEncode)
import Url.Builder exposing (absolute)


type EntityType
    = ResourceType Type
    | EventType Type
    | AgentType Type
    | CommitmentType Type
    | ContractType Type
    | ProcessType Type


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
                , ( "type", T.encode t )
                ]

        EventType t ->
            Encode.object
                [ ( "what", Encode.string "EventType" )
                , ( "type", T.encode t )
                ]

        AgentType t ->
            Encode.object
                [ ( "what", Encode.string "AgentType" )
                , ( "type", T.encode t )
                ]

        CommitmentType t ->
            Encode.object
                [ ( "what", Encode.string "CommitmentType" )
                , ( "type", T.encode t )
                ]

        ContractType t ->
            Encode.object
                [ ( "what", Encode.string "ContractType" )
                , ( "type", T.encode t )
                ]

        ProcessType t ->
            Encode.object
                [ ( "what", Encode.string "ProcessType" )
                , ( "type", T.encode t )
                ]


decoder : Decoder EntityType
decoder =
    Decode.field "what" Decode.string
        |> Decode.andThen
            (\t ->
                Decode.field "type"
                    (case t of
                        "ResourceType" ->
                            Decode.map ResourceType T.decode

                        "EventType" ->
                            Decode.map EventType T.decode

                        "AgentType" ->
                            Decode.map AgentType T.decode

                        "CommitmentType" ->
                            Decode.map CommitmentType T.decode

                        "ContractType" ->
                            Decode.map ContractType T.decode

                        "ProcessType" ->
                            Decode.map ProcessType T.decode

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


compare : EntityType -> String
compare et =
    toString et ++ " " ++ toName et


onlyType : String -> DictSet String EntityType -> DictSet String EntityType
onlyType t ets =
    Set.filter (\et -> toString et == t) ets


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
        , percentEncode <| toName et
        ]
        []
