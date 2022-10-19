module Route exposing (Route(..), firstSegment, redirect, redirectParent, redirectSub, toRoute, toString)

import Browser.Navigation as Nav
import Url exposing (Url, percentEncode)
import Url.Builder as Builder exposing (absolute)
import Url.Parser exposing ((</>), (<?>), Parser, custom, map, oneOf, s, top)
import Url.Parser.Query as Query


type Route
    = Home
      -- Process
    | ProcessTypeList
    | ProcessTypeView String
    | ProcessTypeAdd
    | ProcessTypeEdit String
    | ProcessList (Maybe String)
    | ProcessView String
    | ProcessEdit String
    | ProcessAdd
      -- Resource
    | ResourceTypeList
    | ResourceTypeAdd
    | ResourceTypeEdit String
    | ResourceTypeView String
    | ResourceList
    | ResourceAdd
    | ResourceEdit String
    | ResourceView String
      -- Agent
    | AgentTypeList
    | AgentTypeAdd
    | AgentTypeEdit String
    | AgentTypeView String
    | AgentList
    | AgentAdd
    | AgentEdit String
    | AgentView String
      -- Commitment
    | CommitmentTypeAdd
    | CommitmentTypeList
    | CommitmentTypeEdit String
    | CommitmentTypeView String
    | CommitmentAdd
    | CommitmentList
    | CommitmentEdit String
    | CommitmentView String
      -- Contract
    | ContractTypeList
    | ContractTypeAdd
    | ContractTypeEdit String
    | ContractTypeView String
    | ContractList
    | ContractAdd
    | ContractEdit String
    | ContractView String
      -- Event
    | EventTypeList
    | EventTypeAdd
    | EventTypeEdit String
    | EventTypeView String
    | EventList
    | EventAdd
    | EventEdit String
    | EventView String
      -- Group
    | GroupTypeList
    | GroupTypeAdd
    | GroupTypeEdit String
    | GroupTypeView String
    | GroupList
    | GroupAdd
    | GroupEdit String
    | GroupView String
      -- Ident
    | IdentifierTypeList
    | IdentifierTypeAdd
      -- Value
    | ValueTypeAdd
    | ValueTypeList
      -- Config
    | ConfigurationAdd
    | ConfigurationList


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top

        -- Resource
        , map ResourceTypeList (s "resource-type" </> s "list")
        , map ResourceList (s "resource" </> s "list")
        , map ResourceTypeAdd (s "resource-type" </> s "add")
        , map ResourceAdd (s "resource" </> s "add")
        , map ResourceTypeView (s "resource-type" </> encodedString)
        , map ResourceView (s "resource" </> encodedString)
        , map ResourceEdit (s "resource" </> s "edit" </> encodedString)
        , map ResourceTypeEdit (s "resource-type" </> s "edit" </> encodedString)

        -- Contract
        , map ContractTypeList (s "contract-type" </> s "list")
        , map ContractList (s "contract" </> s "list")
        , map ContractTypeAdd (s "contract-type" </> s "add")
        , map ContractAdd (s "contract" </> s "add")
        , map ContractTypeView (s "contract-type" </> encodedString)
        , map ContractView (s "contract" </> encodedString)
        , map ContractEdit (s "contract" </> s "edit" </> encodedString)
        , map ContractTypeEdit (s "contract-type" </> s "edit" </> encodedString)

        -- Process
        , map ProcessTypeList (s "process-type" </> s "list")
        , map ProcessTypeAdd (s "process-type" </> s "add")
        , map ProcessAdd (s "process" </> s "add")
        , map ProcessList (s "process" </> s "list" <?> Query.string "type")
        , map ProcessTypeView (s "process-type" </> encodedString)
        , map ProcessView (s "process" </> encodedString)
        , map ProcessEdit (s "process" </> s "edit" </> encodedString)
        , map ProcessTypeEdit (s "process-type" </> s "edit" </> encodedString)

        -- Group
        , map GroupTypeList (s "group-type" </> s "list")
        , map GroupTypeAdd (s "group-type" </> s "add")
        , map GroupList (s "group" </> s "list")
        , map GroupAdd (s "group" </> s "add")
        , map GroupTypeView (s "group-type" </> encodedString)
        , map GroupView (s "group" </> encodedString)
        , map GroupEdit (s "group" </> s "edit" </> encodedString)
        , map GroupTypeEdit (s "group-type" </> s "edit" </> encodedString)

        -- Ident
        , map IdentifierTypeAdd (s "identifier-type" </> s "add")
        , map IdentifierTypeList (s "identifier-type" </> s "list")

        -- Value
        , map ValueTypeAdd (s "value-type" </> s "add")
        , map ValueTypeList (s "value-type" </> s "list")

        --, map IdentifierTypeList (s "identifier-type" </> s "list")
        -- Event
        , map EventTypeAdd (s "event-type" </> s "add")
        , map EventAdd (s "event" </> s "add")
        , map EventTypeList (s "event-type" </> s "list")
        , map EventList (s "event" </> s "list")
        , map EventTypeView (s "event-type" </> encodedString)
        , map EventView (s "event" </> encodedString)
        , map EventEdit (s "event" </> s "edit" </> encodedString)
        , map EventTypeEdit (s "event-type" </> s "edit" </> encodedString)

        -- Commitment
        , map CommitmentTypeAdd (s "commitment-type" </> s "add")
        , map CommitmentAdd (s "commitment" </> s "add")
        , map CommitmentTypeList (s "commitment-type" </> s "list")
        , map CommitmentList (s "commitment" </> s "list")
        , map CommitmentTypeView (s "commitment-type" </> encodedString)
        , map CommitmentView (s "commitment" </> encodedString)
        , map CommitmentEdit (s "commitment" </> s "edit" </> encodedString)
        , map CommitmentTypeEdit (s "commitment-type" </> s "edit" </> encodedString)

        -- Agent
        , map AgentTypeAdd (s "agent-type" </> s "add")
        , map AgentAdd (s "agent" </> s "add")
        , map AgentTypeList (s "agent-type" </> s "list")
        , map AgentList (s "agent" </> s "list")
        , map AgentTypeView (s "agent-type" </> encodedString)
        , map AgentView (s "agent" </> encodedString)
        , map AgentTypeEdit (s "agent-type" </> s "edit" </> encodedString)
        , map AgentEdit (s "agent" </> s "edit" </> encodedString)

        -- configure display
        , map ConfigurationList (s "config" </> s "list")
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
            absolute [ "process-type", "list" ] []

        ProcessTypeView ptype ->
            absolute [ "process-type", percentEncode ptype ] []

        ProcessTypeEdit uuid ->
            absolute [ "process-type", "edit", percentEncode uuid ] []

        ProcessTypeAdd ->
            absolute [ "process-type", "add" ] []

        ProcessAdd ->
            absolute [ "process", "add" ] []

        ProcessView p ->
            absolute [ "process", percentEncode p ] []

        ProcessList ps ->
            case ps of
                Just t ->
                    absolute [ "process", "list" ] [ Builder.string "type" t ]

                Nothing ->
                    absolute [ "process", "list" ] []

        ProcessEdit uuid ->
            absolute [ "process", "edit", percentEncode uuid ] []

        ResourceList ->
            absolute [ "resource", "list" ] []

        ResourceView uuid ->
            absolute [ "resource", percentEncode uuid ] []

        ResourceAdd ->
            absolute [ "resource", "add" ] []

        ResourceEdit uuid ->
            absolute [ "resource", "edit", percentEncode uuid ] []

        ResourceTypeView uuid ->
            absolute [ "resource-type", percentEncode uuid ] []

        ResourceTypeEdit uuid ->
            absolute [ "resource-type", "edit", percentEncode uuid ] []

        ResourceTypeAdd ->
            absolute [ "resource-type", "add" ] []

        ResourceTypeList ->
            absolute [ "resource-type", "list" ] []

        EventList ->
            absolute [ "event", "list" ] []

        EventView uuid ->
            absolute [ "event", percentEncode uuid ] []

        EventAdd ->
            absolute [ "event", "add" ] []

        EventEdit uuid ->
            absolute [ "event", "edit", percentEncode uuid ] []

        EventTypeView uuid ->
            absolute [ "event-type", percentEncode uuid ] []

        EventTypeEdit uuid ->
            absolute [ "event-type", "edit", percentEncode uuid ] []

        EventTypeAdd ->
            absolute [ "event-type", "add" ] []

        EventTypeList ->
            absolute [ "event-type", "list" ] []

        AgentList ->
            absolute [ "agent", "list" ] []

        AgentView uuid ->
            absolute [ "agent", percentEncode uuid ] []

        AgentAdd ->
            absolute [ "agent", "add" ] []

        AgentEdit uuid ->
            absolute [ "agent", "edit", percentEncode uuid ] []

        AgentTypeView uuid ->
            absolute [ "agent-type", percentEncode uuid ] []

        AgentTypeEdit uuid ->
            absolute [ "agent-type", "edit", percentEncode uuid ] []

        AgentTypeAdd ->
            absolute [ "agent-type", "add" ] []

        AgentTypeList ->
            absolute [ "agent-type", "list" ] []

        CommitmentList ->
            absolute [ "commitment", "list" ] []

        CommitmentView uuid ->
            absolute [ "commitment", percentEncode uuid ] []

        CommitmentAdd ->
            absolute [ "commitment", "add" ] []

        CommitmentEdit uuid ->
            absolute [ "commitment", "edit", percentEncode uuid ] []

        CommitmentTypeView uuid ->
            absolute [ "commitment-type", percentEncode uuid ] []

        CommitmentTypeEdit uuid ->
            absolute [ "commitment-type", "edit", percentEncode uuid ] []

        CommitmentTypeAdd ->
            absolute [ "commitment-type", "add" ] []

        CommitmentTypeList ->
            absolute [ "commitment-type", "list" ] []

        ContractList ->
            absolute [ "contract", "list" ] []

        ContractView uuid ->
            absolute [ "contract", percentEncode uuid ] []

        ContractAdd ->
            absolute [ "contract", "add" ] []

        ContractEdit uuid ->
            absolute [ "contract", "edit", percentEncode uuid ] []

        ContractTypeView uuid ->
            absolute [ "contract-type", percentEncode uuid ] []

        ContractTypeEdit uuid ->
            absolute [ "contract-type", "edit", percentEncode uuid ] []

        ContractTypeAdd ->
            absolute [ "contract-type", "add" ] []

        ContractTypeList ->
            absolute [ "contract-type", "list" ] []

        GroupList ->
            absolute [ "group", "list" ] []

        GroupView uuid ->
            absolute [ "group", percentEncode uuid ] []

        GroupAdd ->
            absolute [ "group", "add" ] []

        GroupEdit uuid ->
            absolute [ "group", "edit", percentEncode uuid ] []

        GroupTypeView uuid ->
            absolute [ "group-type", percentEncode uuid ] []

        GroupTypeEdit uuid ->
            absolute [ "group-type", "edit", percentEncode uuid ] []

        GroupTypeAdd ->
            absolute [ "group-type", "add" ] []

        GroupTypeList ->
            absolute [ "group-type", "list" ] []

        IdentifierTypeList ->
            absolute [ "identifier-type", "list" ] []

        IdentifierTypeAdd ->
            absolute [ "identifier-type", "add" ] []

        ValueTypeList ->
            absolute [ "value-type", "list" ] []

        ValueTypeAdd ->
            absolute [ "value-type", "add" ] []

        ConfigurationList ->
            absolute [ "config", "list" ] []

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


redirectSub : String -> Nav.Key -> Route -> Cmd msg
redirectSub path navkey route =
    -- redirect to a subpath
    toString route ++ "/" ++ path |> Nav.pushUrl navkey
