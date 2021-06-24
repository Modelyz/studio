module Msg exposing (..)

import Browser
import Time
import Url

type Msg
    = NewProcess
    | NewEvent
    | NewCommitment
    | NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | TimestampEvent Time.Posix
