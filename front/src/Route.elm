module Route exposing (EntitySegment(..), Route(..), ViewSegment(..), all, goBack, redirect, redirectAdd, toDesc, toRoute, toString)

import Browser.Navigation as Nav
import Hierarchy.Type as HType
import Prng.Uuid as Uuid exposing (Uuid)
import State exposing (State)
import Type exposing (Type)
import Typed.Type as TType
import Url exposing (Url, percentEncode)
import Url.Builder as Builder exposing (QueryParameter, absolute)
import Url.Parser exposing ((</>), (<?>), Parser, custom, map, oneOf, s, top)
import Url.Parser.Query as Query
import Zone.View
import Zone.Zone exposing (Zone(..))


type Route
    = Home
    | Entity EntitySegment ViewSegment


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


toType : EntitySegment -> Maybe Type
toType s =
    case s of
        Resource ->
            Just <| Type.HType HType.ResourceType

        Event ->
            Just <| Type.HType HType.EventType

        Agent ->
            Just <| Type.HType HType.AgentType

        Commitment ->
            Just <| Type.HType HType.CommitmentType

        Contract ->
            Just <| Type.HType HType.ContractType

        Process ->
            Just <| Type.HType HType.ProcessType

        Group ->
            Just <| Type.HType HType.GroupType

        _ ->
            Nothing


all : List EntitySegment
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


type ViewSegment
    = View String (Maybe String)
    | Edit String (Maybe String)
    | List (Maybe String)
    | Add (Maybe String)


entityParser : Parser (EntitySegment -> a) a
entityParser =
    oneOf <| List.map (\e -> map e (s (entityToUrl e))) all


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


viewParser : Parser (ViewSegment -> a) a
viewParser =
    oneOf
        [ map View (s "view" </> encodedString <?> Query.string "type")
        , map Add (s "add" <?> Query.string "type")
        , map Edit (s "edit" </> encodedString <?> Query.string "type")
        , map List (s "list" <?> Query.string "type")
        ]


viewToCustomUrl : ViewSegment -> CustomUrl
viewToCustomUrl v =
    case v of
        View s t ->
            CustomUrl [ "view", percentEncode s ] <| Maybe.withDefault [] <| Maybe.map (List.singleton << Builder.string "type") t

        Add t ->
            CustomUrl [ "add" ] <| Maybe.withDefault [] <| Maybe.map (List.singleton << Builder.string "type") t

        Edit s t ->
            CustomUrl [ "edit", percentEncode s ] <| Maybe.withDefault [] <| Maybe.map (List.singleton << Builder.string "type") t

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


toDesc : State -> Route -> String
toDesc s r =
    case r of
        Home ->
            "Home"

        Entity e (List (Just uuid)) ->
            Maybe.map2 (Zone.View.display s.types s.configs SmallcardTitle s.identifiers s.grouped) (toType e) (Uuid.fromString uuid) |> Maybe.withDefault ("Unknown " ++ entityToDesc e)

        Entity e _ ->
            entityToDesc e


redirect : Nav.Key -> Route -> Cmd msg
redirect navkey =
    -- redirect to the specified route
    toString >> Nav.pushUrl navkey


getType : ViewSegment -> Maybe String
getType v =
    case v of
        Add t ->
            t

        Edit _ t ->
            t

        List t ->
            t

        View _ t ->
            t


redirectAdd : Nav.Key -> Route -> Cmd msg
redirectAdd navkey route =
    Nav.pushUrl navkey <|
        toString <|
            case route of
                Entity e v ->
                    Entity e (Add (getType v))

                Home ->
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
                        Edit s t ->
                            Entity e (View s t)

                        View _ t ->
                            Entity e (List t)

                        Add t ->
                            Entity e (List t)

                        List _ ->
                            Home
