module Route exposing (Route(..), parseUrl, routeParser)

import Browser
import Browser.Navigation as Nav
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


type Route
    = NotFound
    | Processes
    | Process String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Processes top
        , map Process (s "process" </> string)
        ]


parseUrl : Url.Url -> Route
parseUrl url =
    let
        route =
            Url.Parser.parse routeParser url
    in
    case route of
        Nothing ->
            NotFound

        Just r ->
            r
