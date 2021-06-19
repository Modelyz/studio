module Msg exposing (..)

import Browser exposing (UrlRequest)
import Url exposing (Url)

type Msg
    = NewSale
    | NoOp
    | LinkClicked UrlRequest
    | UrlChanged Url.Url

