module Page.Process exposing (match, page, view)

import DictSet as Set
import Effect exposing (Effect)
import Element exposing (..)
import Element.Input as Input
import Event exposing (Event(..))
import EventFlow exposing (EventFlow(..))
import IOStatus exposing (IOStatus(..))
import Prng.Uuid as Uuid
import REA.Commitment as C exposing (Commitment)
import REA.CommitmentType exposing (CommitmentType)
import REA.Event as E exposing (Event)
import REA.EventType exposing (EventType)
import REA.Process exposing (Process)
import Route exposing (Route)
import Shared
import Spa.Page
import State exposing (getCommitmentTypes, getCommitments, getEventTypes, getEvents, getProcess)
import View exposing (View)


type alias Model =
    { process : String
    }


type Msg
    = NewCommitment Process String
    | NewEvent Process String


type alias Flags =
    { process : String
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init
        , update = update s
        , view = view s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Process p ->
            Just { process = p }

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init flags =
    ( { process = flags.process
      }
    , Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        NewCommitment process ctype ->
            let
                commitmentType =
                    s.state.commitmentTypes
                        |> Set.toList
                        |> List.filter (\ct -> ct.name == ctype)
                        |> List.head
            in
            case commitmentType of
                Just ct ->
                    ( model
                    , Shared.dispatchT s <|
                        \uuid t -> Event.CommitmentAdded { process = process, commitment = Commitment ct.name uuid t ct }
                    )

                Nothing ->
                    ( model
                    , Effect.none
                    )

        NewEvent process etype ->
            let
                eventType =
                    s.state.eventTypes
                        |> Set.toList
                        |> List.filter (\et -> et.name == etype)
                        |> List.head
            in
            case eventType of
                Just et ->
                    ( model
                    , Shared.dispatchT s <|
                        \uuid t -> Event.EventAdded { process = process, event = E.new et.name uuid t et }
                    )

                Nothing ->
                    ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Process"
    , attributes = []
    , element = viewContent s model
    }


newCommitmentButton : Process -> CommitmentType -> Element Msg
newCommitmentButton process ct =
    Input.button []
        { onPress = Just <| NewCommitment process ct.name
        , label = text ct.name
        }


newEventButton : Process -> EventType -> Element Msg
newEventButton process et =
    Input.button []
        { onPress = Just <| NewEvent process et.name
        , label = text et.name
        }


viewContent : Shared.Model -> Model -> Element Msg
viewContent s model =
    let
        process =
            getProcess s.state model.process

        commitmentTypes =
            process
                |> Maybe.map .type_
                |> Maybe.map (getCommitmentTypes s.state)
                |> Maybe.withDefault (Set.empty .name)

        eventTypes =
            process
                |> Maybe.map .type_
                |> Maybe.map (getEventTypes s.state)
                |> Maybe.withDefault (Set.empty .name)
    in
    case process of
        Nothing ->
            el [ centerX, centerY ] (text "Not Found")

        Just proc ->
            column []
                [ column
                    []
                    [ row []
                        [ paragraph []
                            [ text <| proc.type_ ++ " # " ++ Uuid.toString proc.uuid
                            ]
                        ]
                    ]
                , column []
                    [ row []
                        [ paragraph []
                            [ text "Commitments" ]
                        , column [] <|
                            List.map
                                (newCommitmentButton proc)
                                (s.state.commitmentTypes
                                    |> Set.filter (\ct -> Set.member ct commitmentTypes)
                                    |> Set.toList
                                )
                        , column []
                            [ row []
                                (getCommitments s.state proc
                                    |> Set.toList
                                    |> List.sortBy C.compare
                                    |> List.reverse
                                    |> List.map viewCommitmentThumbnail
                                )
                            ]
                        ]
                    , column []
                        [ paragraph []
                            [ text "Events" ]
                        , column [] <|
                            List.map
                                (newEventButton proc)
                                (s.state.eventTypes
                                    |> Set.filter (\et -> Set.member et eventTypes)
                                    |> Set.toList
                                )
                        , column []
                            [ column []
                                (getEvents s.state proc
                                    |> Set.toList
                                    |> List.sortBy E.compare
                                    |> List.reverse
                                    |> List.map viewEventThumbnail
                                )
                            ]
                        ]
                    ]
                ]


viewCommitmentThumbnail : Commitment -> Element Msg
viewCommitmentThumbnail c =
    row []
        [ link
            []
            { url = "/commitment/" ++ Uuid.toString c.uuid
            , label = text c.name
            }
        , row
            []
            [ text c.name
            , text <| Uuid.toString c.uuid
            ]
        ]


viewEventThumbnail : Event -> Element Msg
viewEventThumbnail c =
    row []
        [ link
            []
            { url = "/event/" ++ Uuid.toString c.uuid
            , label = text c.name
            }
        , row
            []
            [ text c.name
            , text <| Uuid.toString c.uuid
            ]
        ]
