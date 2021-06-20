port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Url
import Url.Parser
import Html exposing (Html, text, div, span, h1, img, button, a, nav)
import Html.Attributes exposing (class, src, width, href, attribute)
import Html.Events exposing (onClick)
import String
import Maybe exposing (Maybe(..))
import Prng.Uuid exposing (Uuid, generator)
import Random.Pcg.Extended exposing (Seed, initialSeed, step)


-- local imports
import Msg
import REA.Process exposing (..)
import REA.Contract exposing (..)
import Route
import NotFound

---- MODEL ----

type alias Model =
    { currentSeed: Seed
    , currentUuid: Maybe Prng.Uuid.Uuid
    , url: Url.Url
    , route: Route.Route
    , navkey: Nav.Key
    , processtype: ProcessType
    , processes: List Process
    }


init : ( Int, List Int ) -> Url.Url -> Nav.Key -> ( Model, Cmd Msg.Msg )
init ( seed, seedExtension ) url navkey =
    ( { currentSeed=initialSeed seed seedExtension
      , currentUuid=Nothing
      , navkey=navkey
      , url=url
      , route=Route.parseUrl url
      , processtype={}
      , processes=[]
      }
    , Cmd.none
    )


-- PORTS --

port storeEvent: String -> Cmd msg


---- UPDATE ----

update : Msg.Msg -> Model -> ( Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.NoOp -> (model, Cmd.none)
        Msg.LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                  ( { model
                    | route=Route.parseUrl url
                    }
                    , Nav.pushUrl model.navkey (Url.toString url) )
                Browser.External href ->
                  ( model, Nav.load href )
        Msg.UrlChanged url ->
            ( { model
              | route = Route.parseUrl url
             }
            , Cmd.none
            )
        Msg.NewSale ->
            let
                ( newUuid, newSeed ) =
                    step Prng.Uuid.generator <| model.currentSeed
                saleId = List.length model.processes + 1
                event = { uuid=model.currentUuid, name="Pizza sale" ++ String.fromInt saleId }
            in
                ( { model
                    | processes=model.processes ++ [ newSale saleId ]
                    , currentUuid = Just newUuid
                    , currentSeed = newSeed
                  }
                , storeEvent <| String.fromInt saleId
                )


newSale : Int -> Process
newSale id =
    Process
    { id=id
    , name="Pizza Sale #" ++ String.fromInt id
    , contract=
        Contract
            { ctype=
                ContractType
                { name="Sale"
                , ctype=Nothing
                }
            , name="Pizza sale #" ++ String.fromInt id
            , parties=[]
            }
    , commitments=[]
    , events=[]
    }


---- VIEW ----


view : Model -> Browser.Document Msg.Msg
view model =
    { title = "Modelyz"
    , body =
        [ nav
            [ class "navbar"
            , attribute "role" "navigation"
            , attribute "aria-label" "main navigation"
            ]
            [ div 
                [ class "navbar-brand"
                ]
                [ a
                    [ class "navbar-item"
                    , href "/"
                    ]
                    [ img
                        [ src "/static/logo.svg"
                        , width 50
                        ] []
                    ]
                , a
                    [ attribute "role" "button"
                    , class "navbar-burger"
                    , attribute "aria-label" "menu"
                    , attribute "aria-expanded" "false"
                    , attribute "dat-target" "navBar"
                    ]
                    [ span [ attribute "aria-hidden" "true" ] [ ]
                    , span [ attribute "aria-hidden" "true" ] [ ]
                    , span [ attribute "aria-hidden" "true" ] [ ]
                    ]
               ]
          ]
        , case model.route of
            Route.NotFound -> NotFound.document
            Route.Home -> 
                div
                    [ class "section"
                    ]
                    [ button
                        [ onClick Msg.NewSale
                        , class "button"
                        ]
                        [ text "New pizza sale"
                        ]
                    , div [class "columns", class "is-multiline"]
                          <| List.map REA.Process.view model.processes
                    ]
            Route.SingleProcess id ->
                div [ ]
                    [ text <| "process" ++ String.fromInt id ]
        ]
    }


onUrlRequest : Browser.UrlRequest -> Msg.Msg
onUrlRequest = Msg.LinkClicked


onUrlChange : Url.Url -> Msg.Msg
onUrlChange = Msg.UrlChanged


---- PROGRAM ----


main : Program (Int, List Int) Model Msg.Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
