port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import DictSet as Set
import ES exposing (Event(..), getProcess)
import Json.Decode exposing (decodeValue, errorToString)
import Json.Encode
import Maybe exposing (Maybe(..))
import Msg exposing (Msg(..))
import Page.CommitmentTypes
import Page.NotFound
import Page.Process
import Page.Processes
import Prng.Uuid as Uuid exposing (generator)
import REA.Commitment as C
import REA.CommitmentType as CT
import REA.Process as P
import Random.Pcg.Extended as Random exposing (initialSeed, step)
import Route exposing (parseUrl)
import Status exposing (Status(..))
import Task
import Time exposing (now)
import Url exposing (Url)


type alias Model =
    ES.State


port receiveEvents : (Json.Encode.Value -> msg) -> Sub msg


port eventStored : (Json.Encode.Value -> msg) -> Sub msg


init : ( Int, List Int ) -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init ( seed, seedExtension ) url navkey =
    ( ES.new (initialSeed seed seedExtension) navkey (parseUrl url)
    , ES.getEvents Json.Encode.null
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EventsReceived results ->
            case decodeValue (Json.Decode.list ES.decoder) results of
                Ok events ->
                    let
                        emptymodel =
                            ES.new model.currentSeed model.navkey model.route
                    in
                    ( List.foldr ES.aggregate { emptymodel | status = Loaded } events, Cmd.none )

                Err str ->
                    ( { model | status = Failed (errorToString str) }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navkey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        -- react to an url change
        UrlChanged url ->
            ( { model | route = parseUrl url }
            , Cmd.none
            )

        NewProcess ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed
            in
            ( { model
                | currentSeed = newSeed
              }
            , Task.perform TimestampEvent <|
                Task.map (\t -> ProcessAdded { uuid = newUuid, posixtime = t, process = P.new newUuid t }) now
            )

        NewCommitment process ctype ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed

                commitmentType =
                    model.commitmentTypes
                        |> Set.toList
                        |> List.filter (\ct -> ct.name == ctype)
                        |> List.head
            in
            ( { model
                | currentSeed = newSeed
              }
            , case commitmentType of
                Just ct ->
                    Task.perform TimestampEvent <|
                        Task.map (\t -> CommitmentAdded { uuid = newUuid, posixtime = t, process = process, commitment = C.new ct.name newUuid t ct }) now

                Nothing ->
                    Cmd.none
            )

        NewCommitmentType name ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator model.currentSeed
            in
            ( { model | inputCommitmentType = "", currentSeed = newSeed }
            , Task.perform TimestampEvent <|
                Task.map (\t -> CommitmentTypeAdded { uuid = newUuid, posixtime = t, commitmentType = CT.new name }) now
            )

        NewEvent process ->
            ( model, Cmd.none )

        TimestampEvent event ->
            ( model, ES.encode event |> ES.storeEvent )

        EventStored _ ->
            ( model, ES.getEvents Json.Encode.null )

        InputCommitmentType ctype ->
            ( { model | inputCommitmentType = ctype }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model.route of
        Route.NotFound ->
            Page.NotFound.view

        Route.Processes ->
            Page.Processes.view model

        Route.Process str ->
            let
                process =
                    getProcess model str
            in
            case process of
                Just p ->
                    Page.Process.view model p

                Nothing ->
                    Page.NotFound.view

        Route.CommitmentTypes ->
            Page.CommitmentTypes.view model


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest =
    LinkClicked


onUrlChange : Url -> Msg
onUrlChange =
    UrlChanged


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ eventStored EventStored
        , receiveEvents EventsReceived
        ]


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
