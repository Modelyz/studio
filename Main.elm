port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (text)
import Json.Encode
import Maybe exposing (Maybe(..))
import Page.Error
import Page.NotFound
import Page.Process
import Page.Processes
import Prng.Uuid exposing (generator)
import REA.ProcessType as PT
import Random.Pcg.Extended exposing (initialSeed, step)
import Route
import Url exposing (Url)


type Msg
    = RouteMsg Route.Msg
    | ProcessesMsg Page.Processes.Msg
    | ProcessMsg Page.Process.Msg


type Model
    = RouteModel Route.Model
    | NotFoundModel
    | ErrorModel
    | ProcessesModel Page.Processes.Model
    | ProcessModel Page.Process.Model


port receiveEvents : (Json.Encode.Value -> msg) -> Sub msg


init : ( Int, List Int ) -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init ( seed, seedExtension ) url navkey =
    let
        ( newUuid, _ ) =
            step generator <| initialSeed seed seedExtension

        route =
            Route.parseUrl url

        session =
            { currentSeed = initialSeed seed seedExtension
            , currentUuid = newUuid
            , navkey = navkey
            , url = url
            , processType = PT.new
            }
    in
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
                    ( NotFoundModel, Cmd.none )

        Route.NotFound ->
            ( NotFoundModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msgtop modeltop =
    case ( msgtop, modeltop ) of
        ( RouteMsg msg, RouteModel model ) ->
            let
                ( modified, cmd ) =
                    Route.update msg model
            in
            ( RouteModel modified, Cmd.map RouteMsg cmd )

        ( ProcessesMsg msg, ProcessesModel model ) ->
            let
                ( modified, cmd ) =
                    Page.Processes.update msg model
            in
            ( ProcessesModel modified, Cmd.map ProcessesMsg cmd )

        ( ProcessMsg msg, ProcessModel model ) ->
            let
                ( modified, cmd ) =
                    Page.Process.update msg model
            in
            ( ProcessModel modified, Cmd.map ProcessMsg cmd )

        ( _, _ ) ->
            ( ErrorModel, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model of
        NotFoundModel ->
            let
                doc =
                    Page.NotFound.view
            in
            { doc
                | body = List.map (Html.map RouteMsg) doc.body
            }

        ErrorModel ->
            let
                doc =
                    Page.Error.view
            in
            { doc
                | body = List.map (Html.map RouteMsg) doc.body
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

        RouteModel _ ->
            { title = "Redirect"
            , body = [ text "FIXME" ]
            }


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest =
    RouteMsg << Route.LinkClicked


onUrlChange : Url -> Msg
onUrlChange =
    RouteMsg << Route.UrlChanged


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        ProcessesModel _ ->
            Sub.map ProcessesMsg (receiveEvents Page.Processes.EventsReceived)

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
