module Route exposing (Route(..), routeParser, toRoute, toUrl)

import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string, top)
import Url.Parser.Query as Query


type Route
    = NotFound Url
    | Home
    | ProcessTypes
    | ProcessType String
    | Processes (Maybe String)
    | Process String
    | CommitmentTypes
    | EventTypes
    | Groups
    | Identifiers


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
        , map Identifiers (s "identifiers")
        ]


toRoute : Url -> Route
toRoute url =
    Url.Parser.parse routeParser url
        |> Maybe.withDefault (NotFound url)


toUrl : Route -> String
toUrl r =
    case r of
        NotFound url ->
            Url.toString url

        Home ->
            "/"

        ProcessTypes ->
            "/process-types"

        ProcessType ptype ->
            "/process-type/" ++ ptype

        Processes mps ->
            case mps of
                Just t ->
                    "/processes?type=" ++ t

                Nothing ->
                    "/processes"

        Process p ->
            "/process/" ++ p

        CommitmentTypes ->
            "/commitment-types"

        EventTypes ->
            "/event-types"

        Groups ->
            "groups"

        Identifiers ->
            "identifiers"
