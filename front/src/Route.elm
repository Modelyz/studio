module Route exposing (Route(..), firstSegment, redirect, redirectAdd, redirectParent, routeParser, toRoute, toString)

import Browser.Navigation as Nav
import Effect exposing (Effect)
import Url exposing (Url, percentEncode)
import Url.Builder as Builder exposing (absolute)
import Url.Parser exposing ((</>), (<?>), Parser, custom, map, oneOf, s, string, top)
import Url.Parser.Query as Query


type Route
    = Home
    | ProcessTypes
    | Processes (Maybe String)
    | Process String
    | ResourceTypes
    | AgentTypes
    | AddProcessType
    | AddResourceType
    | AddEventType
    | AddAgentType
    | AddAgent
    | AddCommitmentType
    | AddContractType
    | ProcessType String
    | AgentType String
    | ContractTypes
    | CommitmentTypes
    | EventTypes
    | Agents
    | GroupTypes
    | AddGroupType
    | Groups
    | AddGroup
    | IdentifierTypes
    | AddIdentifierType


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top

        -- Resource
        , map ResourceTypes (s "resource-types")
        , map AddResourceType (s "resource-types" </> s "add")

        -- Contract
        , map ContractTypes (s "contract-types")
        , map AddContractType (s "contract-types" </> s "add")

        -- Process
        , map ProcessTypes (s "process-types")
        , map AddProcessType (s "process-types" </> s "add")
        , map Processes (s "processes" <?> Query.string "type")
        , map ProcessType (s "process-type" </> encodedString)
        , map Process (s "process" </> encodedString)

        -- Group
        , map GroupTypes (s "group-types")
        , map AddGroupType (s "group-types" </> s "add")
        , map Groups (s "groups")
        , map AddGroup (s "groups" </> s "add")

        -- Ident
        , map AddIdentifierType (s "identifier-types" </> s "add")
        , map IdentifierTypes (s "identifier-types")

        -- Event
        , map AddEventType (s "event-types" </> s "add")
        , map EventTypes (s "event-types")
        , map AddCommitmentType (s "commitment-types" </> s "add")
        , map CommitmentTypes (s "commitment-types")

        -- Agent
        , map AddAgentType (s "agent-types" </> s "add")
        , map AgentTypes (s "agent-types")
        , map AgentType (s "agent-type" </> encodedString)
        , map Agents (s "agents")
        , map AddAgent (s "agents" </> s "add")

        -- edit entities
        ]


encodedString : Parser (String -> a) a
encodedString =
    custom "ENCODED STRING" Url.percentDecode


toRoute : Url -> Route
toRoute url =
    Url.Parser.parse routeParser url
        |> Maybe.withDefault Home


toString : Route -> String
toString r =
    case r of
        Home ->
            absolute [] []

        ProcessTypes ->
            absolute [ "process-types" ] []

        ProcessType ptype ->
            absolute [ "process-type", percentEncode ptype ] []

        Processes ps ->
            case ps of
                Just t ->
                    absolute [ "processes" ] [ Builder.string "type" t ]

                Nothing ->
                    absolute [ "processes" ] []

        ResourceTypes ->
            absolute [ "resource-types" ] []

        EventTypes ->
            absolute [ "event-types" ] []

        Agents ->
            absolute [ "agents" ] []

        AddAgent ->
            absolute [ "agents", "add" ] []

        AgentType at ->
            absolute [ "agent-type", percentEncode at ] []

        AgentTypes ->
            absolute [ "agent-types" ] []

        CommitmentTypes ->
            absolute [ "commitment-types" ] []

        ContractTypes ->
            absolute [ "contract-types" ] []

        AddProcessType ->
            absolute [ "process-types", "add" ] []

        AddResourceType ->
            absolute [ "resource-types", "add" ] []

        AddEventType ->
            absolute [ "event-types", "add" ] []

        AddAgentType ->
            absolute [ "agent-types", "add" ] []

        AddCommitmentType ->
            absolute [ "commitment-types", "add" ] []

        AddContractType ->
            absolute [ "contract-type", "add" ] []

        Process p ->
            absolute [ "process", percentEncode p ] []

        AddGroupType ->
            absolute [ "group-types", "add" ] []

        AddGroup ->
            absolute [ "groups", "add" ] []

        GroupTypes ->
            absolute [ "group-types" ] []

        Groups ->
            absolute [ "groups" ] []

        IdentifierTypes ->
            absolute [ "identifier-types" ] []

        AddIdentifierType ->
            absolute [ "identifier-types", "add" ] []


firstSegment : Route -> String
firstSegment =
    toString >> String.split "/" >> List.drop 1 >> List.head >> Maybe.withDefault "#"


redirect : Nav.Key -> Route -> Cmd msg
redirect navkey =
    -- redirect to the specified route
    toString >> Nav.pushUrl navkey


redirectParent : Nav.Key -> Route -> Cmd msg
redirectParent navkey route =
    -- redirect to the parent route of the specified (one level up the path)
    ("/" ++ firstSegment route) |> Nav.pushUrl navkey


redirectAdd : String -> Nav.Key -> Route -> Cmd msg
redirectAdd path navkey route =
    -- redirect to a subpath
    toString route ++ "/" ++ path |> Nav.pushUrl navkey
