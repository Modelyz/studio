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
    | AddAgentType
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
        , map ProcessTypes (s "process-types")
        , map ProcessType (s "process-type" </> string)
        , map Processes (s "processes" <?> Query.string "type")
        , map Process (s "process" </> string)
        , map CommitmentTypes (s "commitment-types")
        , map EventTypes (s "event-types")
        , map Groups (s "groups")
        , map IdentifierTypes (s "identifierTypes")
        , map AddIdentifierType (s "identifierTypes" </> s "add")
        , map AgentTypes (s "agent-types")
        , map AddAgentType (s "agent-types" </> s "add")
        , map Agents (s "agents")
        , map AddAgent (s "agents" </> s "add")
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

        AgentTypes ->
            "/agent-types"

        AddAgentType ->
            "/agent-types/add"

        ContractTypes ->
            "/contract-types"

        Process p ->
            "/process/" ++ p

        CommitmentTypes ->
            "/commitment-types"

        EventTypes ->
            "/event-types"

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
