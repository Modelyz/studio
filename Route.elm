module Route exposing (Route(..), parseUrl, routeParser)

import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


type Route
    = NotFound
    | Processes
    | Process String
    | CommitmentTypes


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Processes top
        , map Process (s "process" </> string)
        , map CommitmentTypes (s "commitment-types")
        ]


parseUrl : Url.Url -> Route
parseUrl url =
    Url.Parser.parse routeParser url
        |> Maybe.withDefault NotFound
