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
    | EventStored Json.Encode.Value
    | EventsReceived Json.Encode.Value
    | TimestampEvent ES.Event
    | InputProcessName String
    | ProcessTypeChanged ProcessType
    | NewProcess
    | InputCommitmentType String
    | NewCommitmentType String
    | NewCommitment Process String
    | DeleteCommitmentType CommitmentType
    | InputEventType String
    | NewEventType String
    | DeleteEventType EventType
    | NewEvent Process String
