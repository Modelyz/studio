module Msg exposing (Msg(..))

import Browser
import ES
import Json.Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.CommitmentType exposing (CommitmentType)
import REA.Process exposing (Process)
import Url exposing (Url)


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | EventStored Json.Encode.Value
    | EventsReceived Json.Encode.Value
    | TimestampEvent ES.Event
    | NewEvent Process
    | NewCommitment Process String
    | NewCommitmentType String
    | NewProcess
    | InputCommitmentType String
    | DeleteCommitmentType CommitmentType
