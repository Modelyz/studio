module Msg exposing (Msg(..))

import Browser
import ES
import Json.Encode
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
