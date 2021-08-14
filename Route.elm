module Route exposing (Route(..), parseUrl, routeParser)

import Url
import Url.Parser exposing ((</>), Parser, map, (<?>), oneOf, s, string, top)
import Url.Parser.Query as Query


type Route
    = NotFound
    | ProcessTypes
    | ProcessType String
    | Processes (Maybe String)
    | Process String
    | CommitmentTypes
    | EventTypes


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map ProcessTypes (s "process-types")
        , map ProcessType (s "process-type" </> string)
        , map Processes (s "processes" <?> Query.string "type")
        , map Process (s "process" </> string)
        , map CommitmentTypes (s "commitment-types")
        , map EventTypes (s "event-types")
        ]


parseUrl : Url.Url -> Route
parseUrl url =
    Url.Parser.parse routeParser url
        |> Maybe.withDefault NotFound
