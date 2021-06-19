module Route exposing (..)

import Url.Parser exposing (Parser, (</>), top, int, map, oneOf, s, string)
import Url


type Route
    = NotFound
    | Home
    | SingleProcess Int


routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ map Home              top
    , map SingleProcess    (s "process" </> int)
    ]


parseUrl : Url.Url -> Route
parseUrl url
    = let route = Url.Parser.parse routeParser url
    in
        case route of
            Nothing -> NotFound
            Just r -> r
