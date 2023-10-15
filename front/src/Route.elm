module Route exposing (EntitySegment(..), Route(..), ViewSegment(..), allBehaviours, allEntities, allTypes, entityToDesc, goBack, isMenu, redirect, redirectAdd, setHash, toColor, toDesc, toRoute, toString, toTypeFilter)

import Browser.Navigation as Nav
import Configuration as Config exposing (Configuration(..))
import Configuration.Zone exposing (Zone(..))
import Configuration.Zone.View exposing (displayZone)
import Dict
import Element exposing (Color, rgb255)
import Hierarchy.Type as HType
import Prng.Uuid as Uuid exposing (Uuid)
import Scope exposing (Scope(..))
import State exposing (State)
import Type exposing (Type)
import Url exposing (Url, percentEncode)
import Url.Builder as Builder exposing (QueryParameter, Root(..), absolute)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, fragment, map, oneOf, s, top)
import Url.Parser.Query as Query
import Util exposing (otherwise)
import View.Style exposing (color)


type Route
    = Home
    | Entity EntitySegment ViewSegment


type CustomUrl
    = CustomUrl (List String) (List QueryParameter) (Maybe String)


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


type ViewSegment
    = View { uuid : String, type_ : Maybe String }
    | Edit { uuid : String, type_ : Maybe String }
    | List { type_ : Maybe String }
    | Add { type_ : Maybe String, related : Maybe String, step : Maybe String }


toTypeFilter : ViewSegment -> Maybe String
toTypeFilter vs =
    case vs of
        View p ->
            p.type_

        Edit p ->
            p.type_

        List p ->
            p.type_

        Add p ->
            p.type_


toColor : Route -> Color
toColor route =
    case route of
        Home ->
            color.content.background

        Entity Resource _ ->
            rgb255 0xD3 0x87 0xF7

        Entity Event _ ->
            rgb255 0xF7 0x87 0x87

        Entity Agent _ ->
            rgb255 0x87 0x8E 0xF7

        Entity Commitment _ ->
            rgb255 0x87 0xF7 0xF2

        Entity Contract _ ->
            rgb255 0xAC 0xF7 0x87

        Entity Process _ ->
            rgb255 0xF7 0xF0 0x87

        Entity Group _ ->
            rgb255 0xF7 0xA7 0x87

        Entity ResourceType _ ->
            rgb255 0xD3 0x87 0xF7

        Entity EventType _ ->
            rgb255 0xF7 0x87 0x87

        Entity AgentType _ ->
            rgb255 0x87 0x8E 0xF7

        Entity CommitmentType _ ->
            rgb255 0x87 0xF7 0xF2

        Entity ContractType _ ->
            rgb255 0xAC 0xF7 0x87

        Entity ProcessType _ ->
            rgb255 0xF7 0xF0 0x87

        Entity GroupType _ ->
            rgb255 0xF7 0xA7 0x87

        Entity ValueType _ ->
            rgb255 0xF7 0xF7 0xF7

        Entity IdentifierType _ ->
            rgb255 0xF7 0xF7 0xF7

        Entity Configuration _ ->
            rgb255 0xF7 0xF7 0xF7


toHType : EntitySegment -> Maybe Type
toHType s =
    case s of
        ResourceType ->
            Just <| Type.HType HType.ResourceType

        EventType ->
            Just <| Type.HType HType.EventType

        AgentType ->
            Just <| Type.HType HType.AgentType

        CommitmentType ->
            Just <| Type.HType HType.CommitmentType

        ContractType ->
            Just <| Type.HType HType.ContractType

        ProcessType ->
            Just <| Type.HType HType.ProcessType

        GroupType ->
            Just <| Type.HType HType.GroupType

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


allEntities : List EntitySegment
allEntities =
    [ Resource
    , Event
    , Agent
    , Commitment
    , Contract
    , Process
    , Group
    ]


allTypes : List EntitySegment
allTypes =
    [ ResourceType
    , EventType
    , AgentType
    , CommitmentType
    , ContractType
    , ProcessType
    , GroupType
    ]


allBehaviours : List EntitySegment
allBehaviours =
    [ ValueType
    , IdentifierType
    , Configuration
    ]


entityParser : Parser (EntitySegment -> a) a
entityParser =
    oneOf <| List.map (\e -> map e (s (entityToUrl e))) (allEntities ++ allTypes ++ allBehaviours)


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
        [ map (\uuid type_ -> View { uuid = uuid, type_ = type_ }) (s "view" </> encodedString <?> Query.string "type")
        , map (\type_ related step -> Add { type_ = type_, related = related, step = step }) (s "add" <?> Query.string "type" <?> Query.string "related" </> fragment identity)
        , map (\uuid type_ -> Edit { uuid = uuid, type_ = type_ }) (s "edit" </> encodedString <?> Query.string "type")
        , map (\type_ -> List { type_ = type_ }) (s "list" <?> Query.string "type")
        ]


viewToCustomUrl : ViewSegment -> CustomUrl
viewToCustomUrl v =
    case v of
        View p ->
            CustomUrl [ "view", percentEncode p.uuid ] (Maybe.withDefault [] <| Maybe.map (List.singleton << Builder.string "type") p.type_) Nothing

        Add p ->
            CustomUrl [ "add" ]
                (List.filterMap identity
                    [ p.type_ |> Maybe.map (Builder.string "type")
                    , p.related |> Maybe.map (Builder.string "related")
                    ]
                )
                p.step

        Edit p ->
            CustomUrl [ "edit", percentEncode p.uuid ] (Maybe.withDefault [] <| Maybe.map (List.singleton << Builder.string "type") p.type_) Nothing

        List p ->
            CustomUrl [ "list" ] (Maybe.withDefault [] <| Maybe.map (List.singleton << Builder.string "type") p.type_) Nothing


and : CustomUrl -> CustomUrl -> CustomUrl
and (CustomUrl p1s q1s mh1) (CustomUrl p2s q2s mh2) =
    CustomUrl (p1s ++ p2s) (q1s ++ q2s) (mh2 |> otherwise mh1)


setHash : Route -> Maybe Int -> Route
setHash route stepnb =
    case route of
        Entity e v ->
            case v of
                Add p ->
                    Entity e <| Add { p | step = Maybe.map String.fromInt stepnb }

                _ ->
                    route

        _ ->
            route


customToString : CustomUrl -> String
customToString (CustomUrl ps qs mh) =
    Builder.custom Absolute ps qs mh


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Entity (entityParser </> viewParser)
        ]


encodedString : Parser (String -> a) a
encodedString =
    Parser.custom "ENCODED STRING" Url.percentDecode


toRoute : Url -> Route
toRoute url =
    Parser.parse routeParser url
        |> Maybe.withDefault Home


toString : Route -> String
toString route =
    case route of
        Home ->
            absolute [] []

        Entity e v ->
            customToString (viewToCustomUrl v |> and (CustomUrl [ entityToUrl e ] [] Nothing))


toDesc : State -> Route -> String
toDesc s route =
    case route of
        Home ->
            "Home"

        Entity e (List p) ->
            Maybe.map2 (displayZone s MenuZone) (toHType e) (p.type_ |> Maybe.andThen Uuid.fromString) |> Maybe.withDefault ("Unknown " ++ entityToDesc e)

        Entity e _ ->
            entityToDesc e


isMenu : State -> { a | uuid : Uuid, what : HType.Type } -> Bool
isMenu s entity =
    Dict.get (Config.compare (MenuDisplay { what = entity.what, uuid = entity.uuid, isMenu = False })) s.configs
        |> Maybe.map
            (\config ->
                case config of
                    MenuDisplay display ->
                        display.isMenu

                    _ ->
                        True
            )
        |> Maybe.withDefault True


redirect : Nav.Key -> Route -> Cmd msg
redirect navkey =
    -- redirect to the specified route
    toString >> Nav.pushUrl navkey


getType : ViewSegment -> Maybe String
getType v =
    case v of
        Add p ->
            p.type_

        Edit p ->
            p.type_

        List p ->
            p.type_

        View p ->
            p.type_


getRelated : ViewSegment -> Maybe String
getRelated v =
    case v of
        Add p ->
            p.related

        Edit _ ->
            Nothing

        List _ ->
            Nothing

        View _ ->
            Nothing


redirectAdd : Nav.Key -> Route -> Cmd msg
redirectAdd navkey route =
    Nav.pushUrl navkey <|
        toString <|
            case route of
                Entity e v ->
                    -- related is for instance the process linked to the event being added
                    Entity e (Add { type_ = getType v, related = getRelated v, step = Nothing })

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
                        Edit p ->
                            Entity e (View p)

                        View p ->
                            Entity e (List { type_ = p.type_ })

                        Add p ->
                            Entity e (List { type_ = p.type_ })

                        List _ ->
                            Home
