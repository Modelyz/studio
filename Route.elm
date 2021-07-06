module Route exposing (..)

import Browser
import Browser.Navigation as Nav
import Prng.Uuid
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string, top)


type Route
    = NotFound
    | Processes
    | Process String


type alias Model =
    { route : Route
    , navkey : Nav.Key
    }


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model | route = parseUrl url }, Nav.pushUrl model.navkey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        -- react to an url change
        UrlChanged url ->
            ( { model | route = parseUrl url }, Cmd.none )
