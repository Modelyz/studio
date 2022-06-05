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
import REA.EntityType as ENT exposing (EntityType)
import REA.Event as E exposing (Event)
import REA.Process exposing (Process)
import Route exposing (Route)
import Shared
import Spa.Page
import State exposing (getCommitments, getEvents, getProcess, getRestricted)
import View exposing (View, closeMenu)
import View.Navbar as Navbar


type alias Model =
    { route : Route
    , process : String
    }


type Msg
    = NewCommitment Process String
    | NewEvent Process String


type alias Flags =
    { route : Route
    , process : String
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init s
        , update = update s
        , view = view s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Process p ->
            Just { route = route, process = p }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , process = f.process
      }
    , closeMenu s
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        NewCommitment process ctype ->
            let
                commitmentType =
                    s.state.entityTypes
                        |> ENT.onlyType "CommitmentType"
                        |> State.getEntityType ctype
            in
            case commitmentType of
                Just ct ->
                    ( model
                    , Shared.dispatchT s <|
                        \uuid t -> Event.CommitmentAdded (Commitment uuid (ENT.toName ct) t)
                    )

                Nothing ->
                    ( model
                    , Effect.none
                    )

        NewEvent process etype ->
            let
                eventType =
                    s.state.entityTypes
                        |> ENT.onlyType "EventType"
                        |> State.getEntityType etype
            in
            case eventType of
                Just et ->
                    ( model
                    , Shared.dispatchT s <|
                        \uuid t -> Event.EventAdded (E.Event uuid (ENT.toName et) t)
                    )

                Nothing ->
                    ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Process"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


newCommitmentButton : Process -> EntityType -> Element Msg
newCommitmentButton process ct =
    let
        name =
            ENT.toName ct
    in
    Input.button []
        { onPress = Just <| NewCommitment process name
        , label = text name
        }


newEventButton : Process -> EntityType -> Element Msg
newEventButton process et =
    let
        name =
            ENT.toName et
    in
    Input.button []
        { onPress = Just <| NewEvent process name
        , label = text name
        }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        process =
            getProcess s.state model.process

        commitmentTypes =
            process
                |> Maybe.map .type_
                |> Maybe.map (getRestricted "CommitmentType" s.state.restrictions)
                |> Maybe.withDefault (Set.empty ENT.toName)

        eventTypes =
            process
                |> Maybe.map .type_
                |> Maybe.map (getRestricted "EventType" s.state.restrictions)
                |> Maybe.withDefault (Set.empty ENT.toName)
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
                                (s.state.entityTypes
                                    |> ENT.onlyType "CommitmentType"
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
                                (s.state.entityTypes
                                    |> ENT.onlyType "EventType"
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
            , label = text <| Uuid.toString c.uuid
            }
        , row
            []
            [ text <| Uuid.toString c.uuid
            ]
        ]


viewEventThumbnail : Event -> Element Msg
viewEventThumbnail c =
    row []
        [ link
            []
            { url = "/event/" ++ Uuid.toString c.uuid
            , label = text <| Uuid.toString c.uuid
            }
        , row
            []
            [ text <| Uuid.toString c.uuid
            ]
        ]
