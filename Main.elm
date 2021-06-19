module Main exposing (..)

import Browser
import Browser.Navigation as Nav exposing (..)
import Url
import Url.Parser
import Html exposing (Html, text, div, h1, img, button, a)
import Html.Attributes exposing (class, src, width, href)
import Html.Events exposing (onClick)
import String
import Maybe exposing (Maybe(..))

import REA.Process exposing (..)
import REA.Contract exposing (..)
import Msg
import Route
import NotFound

---- MODEL ----

type alias Model =
    { url: Url.Url
    , route: Route.Route
    , navkey: Nav.Key
    , processtype: ProcessType
    , processes: List Process
    }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd msg )
init flags url navkey =
    ( { url=url
      , route=Route.parseUrl url
      , navkey=navkey
      , processtype={}
      , processes=[]
      }
    , Cmd.none
    )




---- UPDATE ----


update : Msg.Msg -> Model -> ( Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.NewSale -> ( {model | processes=model.processes++[newSale <| List.length model.processes + 1]}, Cmd.none)
        Msg.NoOp -> (model, Cmd.none)
        Msg.LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                  ( { model | url=url, route=Route.parseUrl url }
                    , Nav.pushUrl model.navkey (Url.toString url) )
                Browser.External href ->
                  ( model, Nav.load href )
        Msg.UrlChanged url ->
            ( { model
              | url = url
              , route = Route.parseUrl url
             }
            , Nav.pushUrl model.navkey (Url.toString url)
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
        [ div [class "section"]
            [ a [ href "/"]
                [ img [src "/static/logo.svg", width 50] []
                , h1 [] [text "Modelyz"]
            ]]
        , case model.route of
            Route.NotFound -> NotFound.document
            Route.Home -> 
                div [class "section"]
                    [ button [onClick Msg.NewSale] [text "New pizza sale"]
                    , div [class "columns", class "is-multiline"]
                          <| List.map REA.Process.view model.processes
                    ]
            Route.SingleProcess id ->
                div [][text <| "process" ++ String.fromInt id ]
        ]
    }


onUrlRequest : Browser.UrlRequest -> Msg.Msg
onUrlRequest = Msg.LinkClicked


onUrlChange : Url.Url -> Msg.Msg
onUrlChange = Msg.UrlChanged


---- PROGRAM ----


main : Program () Model Msg.Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
