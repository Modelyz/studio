module Route exposing (Msg(..), Route(..), parseUrl, routeParser, update)

import Browser
import Browser.Navigation as Nav
import Session exposing (Session)
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


type Route
    = NotFound
    | Processes
    | Process String


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


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


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( { session | url = url }, Nav.pushUrl session.navkey (Url.toString url) )

                Browser.External href ->
                    ( session, Nav.load href )

        -- react to an url change
        UrlChanged url ->
            ( { session | url = url }, Cmd.none )
