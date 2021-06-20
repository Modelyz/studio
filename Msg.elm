module Msg exposing (..)

import Browser
import Url
import ES

type Msg
    = NewSale
    | NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url

