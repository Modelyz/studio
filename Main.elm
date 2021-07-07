port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
import Json.Encode
import Maybe exposing (Maybe(..))
import Page.Error
import Page.NotFound
import Page.Process
import Page.Processes
import Prng.Uuid exposing (generator)
import REA.ProcessType as PT
import Random.Pcg.Extended exposing (initialSeed, step)
import Route exposing (Route, parseUrl)
import Session exposing (Session)
import Url exposing (Url)


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ProcessesMsg Page.Processes.Msg
    | ProcessMsg Page.Process.Msg


type Model
    = NotFoundModel Session
    | ErrorModel Session
    | ProcessesModel Page.Processes.Model
    | ProcessModel Page.Process.Model


port receiveEvents : (Json.Encode.Value -> msg) -> Sub msg


port eventStored : (Json.Encode.Value -> msg) -> Sub msg


init : ( Int, List Int ) -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init ( seed, seedExtension ) url navkey =
    let
        ( newUuid, _ ) =
            step generator <| initialSeed seed seedExtension

        route =
            parseUrl url

        session =
            { currentSeed = initialSeed seed seedExtension
            , currentUuid = newUuid
            , navkey = navkey
            , url = url
            , processType = PT.new
            }
    in
    toModelCmd route session


toModelCmd : Route -> Session -> ( Model, Cmd Msg )
toModelCmd route session =
    case route of
        Route.Processes ->
            let
                ( model, cmd ) =
                    Page.Processes.init session
            in
            ( ProcessesModel model, Cmd.map ProcessesMsg cmd )

        Route.Process path ->
            case Prng.Uuid.fromString path of
                Just uuid ->
                    let
                        ( model, cmd ) =
                            Page.Process.init uuid session
                    in
                    ( ProcessModel model, Cmd.map ProcessMsg cmd )

                Nothing ->
                    ( NotFoundModel session, Cmd.none )

        Route.NotFound ->
            ( NotFoundModel session, Cmd.none )


toSession : Model -> Session
toSession model =
    case model of
        NotFoundModel session ->
            session

        ErrorModel session ->
            session

        ProcessModel m ->
            m.session

        ProcessesModel m ->
            m.session


update : Msg -> Model -> ( Model, Cmd Msg )
update msgtop modeltop =
    let
        session =
            modeltop |> toSession
    in
    case ( msgtop, modeltop ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        route =
                            parseUrl url
                    in
                    ( toModelCmd route session |> Tuple.first, Nav.pushUrl session.navkey (Url.toString url) )

                Browser.External href ->
                    ( modeltop, Nav.load href )

        -- react to an url change
        ( UrlChanged url, _ ) ->
            toModelCmd (parseUrl url) session

        ( ProcessesMsg msg, ProcessesModel m ) ->
            let
                ( modified, cmd ) =
                    Page.Processes.update msg m
            in
            ( ProcessesModel modified, Cmd.map ProcessesMsg cmd )

        ( ProcessMsg msg, ProcessModel m ) ->
            let
                ( modified, cmd ) =
                    Page.Process.update msg m
            in
            ( ProcessModel modified, Cmd.map ProcessMsg cmd )

        ( _, _ ) ->
            ( ErrorModel session, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model of
        NotFoundModel _ ->
            let
                doc =
                    Page.NotFound.view
            in
            { doc
                | body = doc.body
            }

        ErrorModel _ ->
            let
                doc =
                    Page.Error.view
            in
            { doc
                | body = doc.body
            }

        ProcessesModel m ->
            let
                doc =
                    Page.Processes.view m
            in
            { title = doc.title
            , body = List.map (Html.map ProcessesMsg) doc.body
            }

        ProcessModel m ->
            let
                doc =
                    Page.Process.view m
            in
            { title = doc.title
            , body = List.map (Html.map ProcessMsg) doc.body
            }


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest =
    LinkClicked


onUrlChange : Url -> Msg
onUrlChange =
    UrlChanged


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        ProcessesModel _ ->
            Sub.batch
                [ Sub.map ProcessesMsg (eventStored Page.Processes.EventStored)
                , Sub.map ProcessesMsg (receiveEvents Page.Processes.EventsReceived)
                ]

        ProcessModel _ ->
            Sub.map ProcessMsg (receiveEvents Page.Process.EventsReceived)

        _ ->
            Sub.none


main : Program ( Int, List Int ) Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
