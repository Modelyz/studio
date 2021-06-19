module Msg exposing (..)

import Browser
import Url

type Msg
    = NewSale
    | NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url

