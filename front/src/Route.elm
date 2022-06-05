module Route exposing (Route(..), firstSegment, routeParser, toRoute, toString)

import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string, top)
import Url.Parser.Query as Query


type Route
    = Home
    | ProcessTypes
    | ProcessType String
    | Processes (Maybe String)
    | Process String
    | ResourceTypes
    | AgentTypes
    | AddProcessType
    | AddResourceType
    | AddEventType
    | AddAgentType
    | AddCommitmentType
    | AddContractType
    | ContractTypes
    | CommitmentTypes
    | EventTypes
    | Agents
    | AddAgent
    | Groups
    | IdentifierTypes
    | AddIdentifierType


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top

        -- list types
        , map ResourceTypes (s "resource-types")
        , map EventTypes (s "event-types")
        , map AgentTypes (s "agent-types")
        , map CommitmentTypes (s "commitment-types")
        , map ContractTypes (s "contract-types")
        , map ProcessTypes (s "process-types")

        -- list behaviours
        , map IdentifierTypes (s "identifierTypes")
        , map Groups (s "groups")

        -- add behaviours
        , map AddIdentifierType (s "identifierTypes" </> s "add")

        -- add types
        , map AddResourceType (s "resource-types" </> s "add")
        , map AddEventType (s "event-types" </> s "add")
        , map AddAgentType (s "agent-types" </> s "add")
        , map AddCommitmentType (s "commitment-types" </> s "add")
        , map AddContractType (s "contract-types" </> s "add")
        , map AddProcessType (s "process-types" </> s "add")

        -- add entities
        , map AddAgent (s "agents" </> s "add")

        -- list entities
        , map Agents (s "agents")
        , map Processes (s "processes" <?> Query.string "type")

        -- edit types
        , map ProcessType (s "process-type" </> string)

        -- edit entities
        , map Process (s "process" </> string)
        ]


toRoute : Url -> Route
toRoute url =
    Url.Parser.parse routeParser url
        |> Maybe.withDefault Home


toString : Route -> String
toString r =
    case r of
        Home ->
            "/"

        ProcessTypes ->
            "/process-types"

        ProcessType ptype ->
            "/process-type/" ++ ptype

        Processes ps ->
            case ps of
                Just t ->
                    "/processes?type=" ++ t

                Nothing ->
                    "/processes"

        ResourceTypes ->
            "/resource-types"

        EventTypes ->
            "/event-types"

        AgentTypes ->
            "/agent-types"

        CommitmentTypes ->
            "/commitment-types"

        ContractTypes ->
            "/contract-types"

        AddProcessType ->
            "/process-types/add"

        AddResourceType ->
            "/resource-types/add"

        AddEventType ->
            "/event-types/add"

        AddAgentType ->
            "/agent-types/add"

        AddCommitmentType ->
            "/commitment-types/add"

        AddContractType ->
            "/contract-type/add"

        Process p ->
            "/process/" ++ p

        Groups ->
            "/groups"

        IdentifierTypes ->
            "/identifierTypes"

        AddIdentifierType ->
            "/identifierTypes/add"

        Agents ->
            "/agents"

        AddAgent ->
            "/agents/add"


firstSegment : Route -> String
firstSegment =
    toString >> String.split "/" >> List.drop 1 >> List.head >> Maybe.withDefault "#"
