module Msg exposing (..)

import Browser
import Time
import Url
import Json.Encode

type Msg
    = NewProcess
    | NewEvent
    | NewCommitment
    | NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | TimestampEvent Time.Posix
    | EventsReceived Json.Encode.Value
