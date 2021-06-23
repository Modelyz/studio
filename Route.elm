module Route exposing (..)

import Url.Parser exposing (Parser, (</>), top, int, map, oneOf, s, string)
import Url
import Prng.Uuid


type Route
    = NotFound
    | Home
    | SingleProcess String


routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ map Home              top
    , map SingleProcess    (s "process" </> string)
    ]


parseUrl : Url.Url -> Route
parseUrl url
    = let route = Url.Parser.parse routeParser url
    in
        case route of
            Nothing -> NotFound
            Just r -> r
