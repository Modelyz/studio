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
    | AddCommitmentType
    | AddContractType
    | ProcessType String
    | AgentType String
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
        , map ProcessType (s "process-type" </> encodedString)

        -- edit entities
        , map Process (s "process" </> encodedString)
        , map AgentType (s "agent-type" </> encodedString)
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

        AgentType at ->
            absolute [ "agent-type", percentEncode at ] []

        Process p ->
            absolute [ "process", percentEncode p ] []

        Groups ->
            absolute [ "groups" ] []

        IdentifierTypes ->
            absolute [ "identifierTypes" ] []

        AddIdentifierType ->
            absolute [ "identifierTypes", "add" ] []

        Agents ->
            absolute [ "agents" ] []

        AddAgent ->
            absolute [ "agents", "add" ] []


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
