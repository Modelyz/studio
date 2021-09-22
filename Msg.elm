module Msg exposing (Msg(..))

import Browser
import ES
import Json.Encode
import REA.CommitmentType exposing (CommitmentType)
import REA.EventType exposing (EventType)
import REA.Process exposing (Process)
import REA.ProcessType exposing (ProcessType)
import Url exposing (Url)


type Msg
    = None
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | StoreEvents (List ES.Event)
    | EventStored Json.Encode.Value
    | EventsReceived Json.Encode.Value
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
