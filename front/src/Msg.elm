module Msg exposing (Msg(..))

import Browser
import Event
import Json.Encode
import REA.CommitmentType exposing (CommitmentType)
import REA.EventType exposing (EventType)
import REA.Process exposing (Process)
import REA.ProcessType exposing (ProcessType)
import Url exposing (Url)


type Msg
    = None ()
    | WSDisconnected Json.Encode.Value
    | WSError Json.Encode.Value
    | WSConnect ()
    | WSConnected Json.Encode.Value
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | StoreEventsToSend (List Event.Event)
    | EventsStored Json.Encode.Value
    | EventsStoredTosend Json.Encode.Value
    | EventsRead Json.Encode.Value
    | InputProcessName String
    | ProcessTypeChanged ProcessType
    | DeleteProcessType ProcessType
    | NewProcess ProcessType
    | InputCommitmentType String
    | InputCommitmentTypeProcessType String
    | NewCommitmentType String
    | NewCommitment Process String
    | DeleteCommitmentType CommitmentType
    | InputEventType String
    | InputEventTypeProcessType String
    | NewEventType
    | DeleteEventType EventType
    | NewEvent Process String
    | EventsSent Json.Encode.Value
    | EventsReceived String
