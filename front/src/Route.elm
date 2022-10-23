module Route exposing (Route(..), firstSegment, redirect, redirectView, redirectViewItem, toRoute, toString)

import Browser.Navigation as Nav
import Prng.Uuid as Uuid exposing (Uuid)
import Url exposing (Url, percentEncode)
import Url.Builder as Builder exposing (absolute)
import Url.Parser exposing ((</>), (<?>), Parser, custom, map, oneOf, s, string, top)
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
    | IdentifierTypeEdit String
    | IdentifierTypeView String
      -- Value
    | ValueTypeAdd
    | ValueTypeList
    | ValueTypeEdit String
    | ValueTypeView String
      -- Config
    | ConfigurationAdd
    | ConfigurationList
    | ConfigurationEdit String
    | ConfigurationView String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top

        -- Agent
        , map AgentAdd (s "agent" </> s "add")
        , map AgentEdit (s "agent" </> s "edit" </> encodedString)
        , map AgentList (s "agent" </> s "list")
        , map AgentView (s "agent" </> s "view" </> encodedString)
        , map AgentTypeAdd (s "agent-type" </> s "add")
        , map AgentTypeEdit (s "agent-type" </> s "edit" </> encodedString)
        , map AgentTypeList (s "agent-type" </> s "list")
        , map AgentTypeView (s "agent-type" </> s "view" </> encodedString)

        -- Commitment
        , map CommitmentAdd (s "commitment" </> s "add")
        , map CommitmentEdit (s "commitment" </> s "edit" </> encodedString)
        , map CommitmentList (s "commitment" </> s "list")
        , map CommitmentView (s "commitment" </> s "view" </> encodedString)
        , map CommitmentTypeAdd (s "commitment-type" </> s "add")
        , map CommitmentTypeEdit (s "commitment-type" </> s "edit" </> encodedString)
        , map CommitmentTypeList (s "commitment-type" </> s "list")
        , map CommitmentTypeView (s "commitment-type" </> s "view" </> encodedString)

        -- configure display
        , map ConfigurationAdd (s "config" </> s "add")
        , map ConfigurationEdit (s "config" </> s "edit" </> encodedString)
        , map ConfigurationList (s "config" </> s "list")
        , map ConfigurationView (s "config" </> s "view" </> encodedString)

        -- Contract
        , map ContractAdd (s "contract" </> s "add")
        , map ContractEdit (s "contract" </> s "edit" </> encodedString)
        , map ContractList (s "contract" </> s "list")
        , map ContractView (s "contract" </> s "view" </> encodedString)
        , map ContractTypeAdd (s "contract-type" </> s "add")
        , map ContractTypeEdit (s "contract-type" </> s "edit" </> encodedString)
        , map ContractTypeList (s "contract-type" </> s "list")
        , map ContractTypeView (s "contract-type" </> s "view" </> encodedString)

        -- Event
        , map EventAdd (s "event" </> s "add")
        , map EventEdit (s "event" </> s "edit" </> encodedString)
        , map EventList (s "event" </> s "list")
        , map EventView (s "event" </> s "view" </> encodedString)
        , map EventTypeAdd (s "event-type" </> s "add")
        , map EventTypeEdit (s "event-type" </> s "edit" </> encodedString)
        , map EventTypeList (s "event-type" </> s "list")
        , map EventTypeView (s "event-type" </> s "view" </> encodedString)

        -- Group
        , map GroupAdd (s "group" </> s "add")
        , map GroupEdit (s "group" </> s "edit" </> encodedString)
        , map GroupList (s "group" </> s "list")
        , map GroupView (s "group" </> s "view" </> encodedString)
        , map GroupTypeAdd (s "group-type" </> s "add")
        , map GroupTypeEdit (s "group-type" </> s "edit" </> encodedString)
        , map GroupTypeList (s "group-type" </> s "list")
        , map GroupTypeView (s "group-type" </> s "view" </> encodedString)

        -- Ident
        , map IdentifierTypeAdd (s "identifier-type" </> s "add")
        , map IdentifierTypeEdit (s "identifier-type" </> s "edit" </> encodedString)
        , map IdentifierTypeList (s "identifier-type" </> s "list")
        , map IdentifierTypeView (s "identifier-type" </> s "view" </> encodedString)

        -- Process
        , map ProcessAdd (s "process" </> s "add")
        , map ProcessEdit (s "process" </> s "edit" </> encodedString)
        , map ProcessList (s "process" </> s "list" <?> Query.string "type")
        , map ProcessView (s "process" </> s "view" </> encodedString)
        , map ProcessTypeAdd (s "process-type" </> s "add")
        , map ProcessTypeEdit (s "process-type" </> s "edit" </> encodedString)
        , map ProcessTypeList (s "process-type" </> s "list")
        , map ProcessTypeView (s "process-type" </> s "view" </> encodedString)

        -- Resource
        , map ResourceAdd (s "resource" </> s "add")
        , map ResourceEdit (s "resource" </> s "edit" </> encodedString)
        , map ResourceList (s "resource" </> s "list")
        , map ResourceView (s "resource" </> s "view" </> encodedString)
        , map ResourceTypeAdd (s "resource-type" </> s "add")
        , map ResourceTypeEdit (s "resource-type" </> s "edit" </> encodedString)
        , map ResourceTypeList (s "resource-type" </> s "list")
        , map ResourceTypeView (s "resource-type" </> s "view" </> encodedString)

        -- Value
        , map ValueTypeAdd (s "value-type" </> s "add")
        , map ValueTypeEdit (s "value-type" </> s "edit" </> encodedString)
        , map ValueTypeList (s "value-type" </> s "list")
        , map ValueTypeView (s "value-type" </> s "view" </> encodedString)
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
            absolute [ "process-type", "view", percentEncode ptype ] []

        ProcessTypeEdit uuid ->
            absolute [ "process-type", "edit", percentEncode uuid ] []

        ProcessTypeAdd ->
            absolute [ "process-type", "add" ] []

        ProcessAdd ->
            absolute [ "process", "add" ] []

        ProcessView p ->
            absolute [ "process", "view", percentEncode p ] []

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
            absolute [ "resource", "view", percentEncode uuid ] []

        ResourceAdd ->
            absolute [ "resource", "add" ] []

        ResourceEdit uuid ->
            absolute [ "resource", "edit", percentEncode uuid ] []

        ResourceTypeView uuid ->
            absolute [ "resource-type", "view", percentEncode uuid ] []

        ResourceTypeEdit uuid ->
            absolute [ "resource-type", "edit", percentEncode uuid ] []

        ResourceTypeAdd ->
            absolute [ "resource-type", "add" ] []

        ResourceTypeList ->
            absolute [ "resource-type", "list" ] []

        EventList ->
            absolute [ "event", "list" ] []

        EventView uuid ->
            absolute [ "event", "view", percentEncode uuid ] []

        EventAdd ->
            absolute [ "event", "add" ] []

        EventEdit uuid ->
            absolute [ "event", "edit", percentEncode uuid ] []

        EventTypeView uuid ->
            absolute [ "event-type", "view", percentEncode uuid ] []

        EventTypeEdit uuid ->
            absolute [ "event-type", "edit", percentEncode uuid ] []

        EventTypeAdd ->
            absolute [ "event-type", "add" ] []

        EventTypeList ->
            absolute [ "event-type", "list" ] []

        AgentList ->
            absolute [ "agent", "list" ] []

        AgentView uuid ->
            absolute [ "agent", "view", percentEncode uuid ] []

        AgentAdd ->
            absolute [ "agent", "add" ] []

        AgentEdit uuid ->
            absolute [ "agent", "edit", percentEncode uuid ] []

        AgentTypeView uuid ->
            absolute [ "agent-type", "view", percentEncode uuid ] []

        AgentTypeEdit uuid ->
            absolute [ "agent-type", "edit", percentEncode uuid ] []

        AgentTypeAdd ->
            absolute [ "agent-type", "add" ] []

        AgentTypeList ->
            absolute [ "agent-type", "list" ] []

        CommitmentList ->
            absolute [ "commitment", "list" ] []

        CommitmentView uuid ->
            absolute [ "commitment", "view", percentEncode uuid ] []

        CommitmentAdd ->
            absolute [ "commitment", "add" ] []

        CommitmentEdit uuid ->
            absolute [ "commitment", "edit", percentEncode uuid ] []

        CommitmentTypeView uuid ->
            absolute [ "commitment-type", "view", percentEncode uuid ] []

        CommitmentTypeEdit uuid ->
            absolute [ "commitment-type", "edit", percentEncode uuid ] []

        CommitmentTypeAdd ->
            absolute [ "commitment-type", "add" ] []

        CommitmentTypeList ->
            absolute [ "commitment-type", "list" ] []

        ContractList ->
            absolute [ "contract", "list" ] []

        ContractView uuid ->
            absolute [ "contract", "view", percentEncode uuid ] []

        ContractAdd ->
            absolute [ "contract", "add" ] []

        ContractEdit uuid ->
            absolute [ "contract", "edit", percentEncode uuid ] []

        ContractTypeView uuid ->
            absolute [ "contract-type", "view", percentEncode uuid ] []

        ContractTypeEdit uuid ->
            absolute [ "contract-type", "edit", percentEncode uuid ] []

        ContractTypeAdd ->
            absolute [ "contract-type", "add" ] []

        ContractTypeList ->
            absolute [ "contract-type", "list" ] []

        GroupList ->
            absolute [ "group", "list" ] []

        GroupView uuid ->
            absolute [ "group", "view", percentEncode uuid ] []

        GroupAdd ->
            absolute [ "group", "add" ] []

        GroupEdit uuid ->
            absolute [ "group", "edit", percentEncode uuid ] []

        GroupTypeView uuid ->
            absolute [ "group-type", "view", percentEncode uuid ] []

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

        IdentifierTypeView itid ->
            absolute [ "identifier-type", "view", percentEncode itid ] []

        IdentifierTypeEdit itid ->
            absolute [ "identifier-type", "edit", percentEncode itid ] []

        ValueTypeList ->
            absolute [ "value-type", "list" ] []

        ValueTypeAdd ->
            absolute [ "value-type", "add" ] []

        ValueTypeView vtid ->
            absolute [ "value-type", "view", percentEncode vtid ] []

        ValueTypeEdit vtid ->
            absolute [ "value-type", "edit", percentEncode vtid ] []

        ConfigurationList ->
            absolute [ "config", "list" ] []

        ConfigurationAdd ->
            absolute [ "config", "add" ] []

        ConfigurationView zid ->
            absolute [ "config", "view", percentEncode zid ] []

        ConfigurationEdit zid ->
            absolute [ "config", "edit", percentEncode zid ] []


firstSegment : Route -> String
firstSegment =
    toString >> String.split "/" >> List.drop 1 >> List.head >> Maybe.withDefault "#"


upper : Route -> String
upper =
    toString >> String.split "/" >> List.reverse >> List.drop 1 >> List.reverse >> String.join "/"


redirect : Nav.Key -> Route -> Cmd msg
redirect navkey =
    -- redirect to the specified route
    toString >> Nav.pushUrl navkey



-- TODO avoid using String


redirectViewItem : String -> String -> Nav.Key -> Route -> Cmd msg
redirectViewItem view item navkey route =
    -- redirect to /<view>/<item>/
    "/" ++ firstSegment route ++ "/" ++ view ++ "/" ++ item |> Nav.pushUrl navkey


redirectView : String -> Nav.Key -> Route -> Cmd msg
redirectView view navkey =
    -- replaces /*/<item> with /<view>/<item>
    toString
        >> String.split "/"
        >> List.indexedMap
            (\i segment ->
                if i == 2 then
                    view

                else
                    segment
            )
        >> String.join "/"
        >> Nav.pushUrl navkey
