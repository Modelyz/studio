module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, button)
import Html.Attributes exposing (src, width)
import Html.Events exposing (onClick)
import String


---- MODEL ----


type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )



---- UPDATE ----


type Msg
    = Plus | Moins


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Plus -> ( model + 1, Cmd.none)
        Moins -> ( model - 1, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "logo.svg", width 50 ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , button [onClick Plus] [text "+"]
        , button [onClick Moins] [text "-"]
        , text <| String.fromInt model
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
