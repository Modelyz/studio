module Route exposing (Route(..), firstSegment, redirect, redirectAdd, redirectParent, routeParser, toRoute, toString)

import Browser.Navigation as Nav
import Effect exposing (Effect)
import Url exposing (Url, percentEncode)
import Url.Builder as Builder exposing (absolute)
import Url.Parser exposing ((</>), (<?>), Parser, custom, map, oneOf, s, string, top)
import Url.Parser.Query as Query


type Route
    = Home
      -- Process
    | ProcessTypeList
    | ProcessList (Maybe String)
    | ProcessView String
    | ProcessTypeView String
    | ProcessTypeAdd
    | ProcessAdd
      -- Resource
    | ResourceTypeList
    | ResourceList
    | ResourceTypeAdd
    | ResourceAdd
      -- Agent
    | AgentTypeList
    | AgentTypeAdd
    | AgentList
    | AgentAdd
    | AgentTypeView String
      -- Commitment
    | CommitmentTypeAdd
    | CommitmentAdd
    | CommitmentTypeList
    | CommitmentList
      -- Contract
    | ContractTypeAdd
    | ContractAdd
    | ContractTypeList
    | ContractList
      -- Event
    | EventTypeList
    | EventList
    | EventTypeAdd
    | EventAdd
      -- Group
    | GroupTypeList
    | GroupTypeAdd
    | GroupList
    | GroupAdd
      -- Ident
    | IdentifierTypeList
    | IdentifierTypeAdd
      -- Config
    | ConfigurationAdd
    | ConfigurationList


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top

        -- Resource
        , map ResourceTypeList (s "resource-type")
        , map ResourceList (s "resource")
        , map ResourceTypeAdd (s "resource-type" </> s "add")
        , map ResourceAdd (s "resource" </> s "add")

        -- Contract
        , map ContractTypeList (s "contract-type")
        , map ContractList (s "contract")
        , map ContractTypeAdd (s "contract-type" </> s "add")
        , map ContractAdd (s "contract" </> s "add")

        -- Process
        , map ProcessTypeList (s "process-type")
        , map ProcessTypeAdd (s "process-type" </> s "add")
        , map ProcessAdd (s "process" </> s "add")
        , map ProcessList (s "process" <?> Query.string "type")
        , map ProcessTypeView (s "process-type" </> encodedString)
        , map ProcessView (s "process" </> encodedString)

        -- Group
        , map GroupTypeList (s "group-type")
        , map GroupTypeAdd (s "group-type" </> s "add")
        , map GroupList (s "group")
        , map GroupAdd (s "group" </> s "add")

        -- Ident
        , map IdentifierTypeAdd (s "identifier-type" </> s "add")
        , map IdentifierTypeList (s "identifier-type")

        -- Event
        , map EventTypeAdd (s "event-type" </> s "add")
        , map EventAdd (s "event" </> s "add")
        , map EventTypeList (s "event-type")
        , map EventList (s "event")

        -- Commitment
        , map CommitmentTypeAdd (s "commitment-type" </> s "add")
        , map CommitmentAdd (s "commitment" </> s "add")
        , map CommitmentTypeList (s "commitment-type")
        , map CommitmentList (s "commitment")

        -- Agent
        , map AgentTypeAdd (s "agent-type" </> s "add")
        , map AgentTypeList (s "agent-type")
        , map AgentTypeView (s "agent-type" </> encodedString)
        , map AgentList (s "agent")
        , map AgentAdd (s "agent" </> s "add")

        -- configure display
        , map ConfigurationList (s "config")
        , map ConfigurationAdd (s "config" </> s "add")
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

        ProcessTypeList ->
            absolute [ "process-type" ] []

        ProcessTypeView ptype ->
            absolute [ "process-type", percentEncode ptype ] []

        ProcessAdd ->
            absolute [ "process", "add" ] []

        ProcessTypeAdd ->
            absolute [ "process-type", "add" ] []

        ProcessView p ->
            absolute [ "process", percentEncode p ] []

        ProcessList ps ->
            case ps of
                Just t ->
                    absolute [ "process" ] [ Builder.string "type" t ]

                Nothing ->
                    absolute [ "process" ] []

        ResourceList ->
            absolute [ "resource" ] []

        ResourceAdd ->
            absolute [ "resource", "add" ] []

        ResourceTypeList ->
            absolute [ "resource-type" ] []

        ResourceTypeAdd ->
            absolute [ "resource-type", "add" ] []

        EventTypeAdd ->
            absolute [ "event-type", "add" ] []

        EventTypeList ->
            absolute [ "event-type" ] []

        EventAdd ->
            absolute [ "event", "add" ] []

        EventList ->
            absolute [ "event" ] []

        AgentList ->
            absolute [ "agent" ] []

        AgentAdd ->
            absolute [ "agent", "add" ] []

        AgentTypeView at ->
            absolute [ "agent-type", percentEncode at ] []

        AgentTypeAdd ->
            absolute [ "agent-type", "add" ] []

        AgentTypeList ->
            absolute [ "agent-type" ] []

        CommitmentTypeList ->
            absolute [ "commitment-type" ] []

        CommitmentTypeAdd ->
            absolute [ "commitment-type", "add" ] []

        CommitmentList ->
            absolute [ "commitment" ] []

        CommitmentAdd ->
            absolute [ "commitment", "add" ] []

        ContractTypeList ->
            absolute [ "contract-type" ] []

        ContractTypeAdd ->
            absolute [ "contract-type", "add" ] []

        ContractList ->
            absolute [ "contract" ] []

        ContractAdd ->
            absolute [ "contract", "add" ] []

        GroupTypeAdd ->
            absolute [ "group-type", "add" ] []

        GroupAdd ->
            absolute [ "group", "add" ] []

        GroupTypeList ->
            absolute [ "group-type" ] []

        GroupList ->
            absolute [ "group" ] []

        IdentifierTypeList ->
            absolute [ "identifier-type" ] []

        IdentifierTypeAdd ->
            absolute [ "identifier-type", "add" ] []

        ConfigurationList ->
            absolute [ "config" ] []

        ConfigurationAdd ->
            absolute [ "config", "add" ] []


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
