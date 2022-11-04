module Route exposing (EntitySegment(..), Route(..), View(..), all, entityToString, goBack, redirect, redirectAdd, redirectView, toDesc, toRoute, toString)

import Browser.Navigation as Nav
import Prng.Uuid as Uuid exposing (Uuid)
import Url exposing (Url, percentEncode)
import Url.Builder as Builder exposing (QueryParameter, absolute)
import Url.Parser exposing ((</>), (<?>), Parser, custom, map, oneOf, s, string, top)
import Url.Parser.Query as Query


type Route
    = Home
    | Entity EntitySegment View


type CustomUrl
    = CustomUrl (List String) (List QueryParameter)


type EntitySegment
    = Resource
    | Event
    | Agent
    | Commitment
    | Contract
    | Process
    | Group
    | ResourceType
    | EventType
    | AgentType
    | CommitmentType
    | ContractType
    | ProcessType
    | GroupType
    | ValueType
    | IdentifierType
    | Configuration


all =
    [ Resource
    , Event
    , Agent
    , Commitment
    , Contract
    , Process
    , Group
    , ResourceType
    , EventType
    , AgentType
    , CommitmentType
    , ContractType
    , ProcessType
    , GroupType
    , ValueType
    , IdentifierType
    , Configuration
    ]


type View
    = View String
    | Edit String
    | List (Maybe String)
    | Add


entityParser : Parser (EntitySegment -> a) a
entityParser =
    oneOf <| List.map (\e -> map e (s (entityToUrl e))) all


entityToString : EntitySegment -> String
entityToString e =
    case e of
        Resource ->
            "Resource"

        Event ->
            "Event"

        Agent ->
            "Agent"

        Commitment ->
            "Commitment"

        Contract ->
            "Contract"

        Process ->
            "Process"

        Group ->
            "Group"

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

        ProcessType ->
            "ProcessType"

        GroupType ->
            "GroupType"

        ValueType ->
            "ValueType"

        IdentifierType ->
            "IdentifierType"

        Configuration ->
            "Configuration"


entityToDesc : EntitySegment -> String
entityToDesc e =
    case e of
        Resource ->
            "Resources"

        Event ->
            "Events"

        Agent ->
            "Agents"

        Commitment ->
            "Commitments"

        Contract ->
            "Contracts"

        Process ->
            "Processes"

        Group ->
            "Groups"

        ResourceType ->
            "Resource Types"

        EventType ->
            "Event Types"

        AgentType ->
            "Agent Types"

        CommitmentType ->
            "Commitment Types"

        ContractType ->
            "Contract Types"

        ProcessType ->
            "Process Types"

        GroupType ->
            "Group Types"

        ValueType ->
            "Value Types"

        IdentifierType ->
            "Identifier Types"

        Configuration ->
            "Configurations"


entityToUrl : EntitySegment -> String
entityToUrl e =
    case e of
        Resource ->
            "resource"

        Event ->
            "event"

        Agent ->
            "agent"

        Commitment ->
            "commitment"

        Contract ->
            "contract"

        Process ->
            "process"

        Group ->
            "group"

        ResourceType ->
            "resource-type"

        EventType ->
            "event-type"

        AgentType ->
            "agent-type"

        CommitmentType ->
            "commitment-type"

        ContractType ->
            "contract-type"

        ProcessType ->
            "process-type"

        GroupType ->
            "group-type"

        ValueType ->
            "value-type"

        IdentifierType ->
            "identifier-type"

        Configuration ->
            "configuration"


viewParser : Parser (View -> a) a
viewParser =
    oneOf
        [ map View (s "view" </> encodedString)
        , map Add (s "add")
        , map Edit (s "edit" </> encodedString)
        , map List (s "list" <?> Query.string "type")
        ]


viewToCustomUrl : View -> CustomUrl
viewToCustomUrl v =
    case v of
        View s ->
            CustomUrl [ "view", percentEncode s ] []

        Add ->
            CustomUrl [ "add" ] []

        Edit s ->
            CustomUrl [ "edit", percentEncode s ] []

        List t ->
            CustomUrl [ "list" ] <| Maybe.withDefault [] <| Maybe.map (List.singleton << Builder.string "type") t


and : CustomUrl -> CustomUrl -> CustomUrl
and (CustomUrl p1s q1s) (CustomUrl p2s q2s) =
    CustomUrl (p1s ++ p2s) (q1s ++ q2s)


customToString : CustomUrl -> String
customToString (CustomUrl ps qs) =
    absolute ps qs


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Entity (entityParser </> viewParser)
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

        Entity e v ->
            customToString (viewToCustomUrl v |> and (CustomUrl [ entityToUrl e ] []))


toDesc : Route -> String
toDesc r =
    case r of
        Home ->
            "Home"

        Entity e v ->
            entityToDesc e


upper : Route -> String
upper =
    toString >> String.split "/" >> List.reverse >> List.drop 1 >> List.reverse >> String.join "/"


redirect : Nav.Key -> Route -> Cmd msg
redirect navkey =
    -- redirect to the specified route
    toString >> Nav.pushUrl navkey


redirectAdd : Nav.Key -> Route -> Cmd msg
redirectAdd navkey route =
    Nav.pushUrl navkey <|
        toString <|
            case route of
                Entity e _ ->
                    Entity e Add

                Home ->
                    Home


redirectView : Nav.Key -> Route -> Cmd msg
redirectView navkey route =
    Nav.pushUrl navkey <|
        toString <|
            case route of
                Entity e (Edit s) ->
                    Entity e (View s)

                Entity e (View s) ->
                    Entity e (View s)

                _ ->
                    Home


goBack : Nav.Key -> Route -> Cmd msg
goBack navkey route =
    Nav.pushUrl navkey <|
        toString <|
            case route of
                Home ->
                    Home

                Entity e v ->
                    case v of
                        Edit s ->
                            Entity e (View s)

                        View s ->
                            Entity e (List Nothing)

                        Add ->
                            Entity e (List Nothing)

                        List _ ->
                            Home
