module Main exposing (..)

import Browser exposing (Document, application, UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Html exposing (Html, text, div, h1, img, button)
import Html.Attributes exposing (class, src, width)
import Html.Events exposing (onClick)
import String
import Maybe exposing (Maybe(..))

import Instance exposing (..)
import Contract exposing (..)
import Msg exposing (..)

---- MODEL ----

type alias Pattern =
    {}

type alias Model =
    { pattern: Pattern,  instances: List Instance }


init : flags -> Url -> Key -> ( Model, Cmd msg )
init flags url key =
    ( {pattern={}, instances=[]}, Cmd.none )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.NewSale -> ( {model | instances=model.instances++[newSale <| List.length model.instances + 1]}, Cmd.none)
        Msg.NoOp -> (model, Cmd.none)


newSale : Int -> Instance
newSale id =
    Instance
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


view : Model -> Document Msg
view model =
    { title = "Modelyz"
    , body =
        [div [class "section"]
            [img [src "logo.svg", width 50] []
            , h1 [] [text "Modelyz"]
            , button [onClick Msg.NewSale] [text "New pizza sale"]
            ]
        , div [class "columns", class "is-multiline"]
              <| List.map Instance.view model.instances
        ] }


onUrlRequest : UrlRequest -> Msg
onUrlRequest url = Msg.NoOp


onUrlChange : Url -> Msg
onUrlChange url = Msg.NoOp


---- PROGRAM ----


main : Program () Model Msg
main =
    application
        { init = init
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
