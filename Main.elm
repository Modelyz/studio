module Main exposing (..)

import Browser exposing (Document, application, UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Html exposing (Html, text, div, h1, img, button)
import Html.Attributes exposing (class, src, width)
import Html.Events exposing (onClick)
import String
import Maybe exposing (Maybe(..))


---- MODEL ----

type ContractType =
    ContractType
    { name: String
    , ctype: Maybe ContractType
    }

type AgentType =
    AgentType
        { name: String
        , atype: Maybe AgentType
         }

type CommitmentType =
    CommitmentType
        { name: String
        , ctype: Maybe CommitmentType
         }

type EventType =
    EventType
        { name: String
        , etype: Maybe EventType
         }

type ResourceType =
    ResourceType
        { name: String
        , rtype: Maybe ResourceType
        }

type Agent =
    Agent
        { name: String
        , atype: AgentType
         }

type Contract =
    Contract
    { name: String
    , ctype: ContractType
    , parties: List Agent
--    , clauses: 
--    , terms: 
    }

type Commitment =
    Commitment
        { name: String
        , ctype: CommitmentType
        , qty: Float
        , rtype: ResourceType
        , provider: Agent
        , receiver: Agent }

type Event =
    Event
        { name: String
        , etype: EventType
        , qty: Float
        , rtype: ResourceType
        , provider: Agent
        , receiver: Agent }

type Instance =
    Instance
        { id: Int
        , name: String
        , contract: Contract
        , commitments: List Commitment
        , events: List Event
        }

type alias Pattern =
    {}

type alias Model =
    { pattern: Pattern,  instances: List Instance }


init : flags -> Url -> Key -> ( Model, Cmd msg )
init flags url key =
    ( {pattern={}, instances=[]}, Cmd.none )



---- UPDATE ----


type Msg
    = NewSale
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSale -> ( {model | instances=model.instances++[newSale <| List.length model.instances + 1]}, Cmd.none)
        NoOp -> (model, Cmd.none)


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
    , events=[]}


---- VIEW ----

view_instance: Instance -> Html Msg
view_instance i =
    div [ class "column", class "is-one-quarter" ]
        [ div [ class "box" ]
              [ text <| (\(Instance ii) -> ii.name) i ]
        ]


view : Model -> Document Msg
view model =
    { title = "Modelyz"
    , body =
        [ div [ class "section" ]
            [ img [ src "logo.svg", width 50 ] []
            , h1 [] [ text "Modelyz" ]
            , button [onClick NewSale] [text "New pizza sale"]
            ]
        , div [ class "columns", class "is-multiline" ]
                    <| List.map view_instance model.instances
        ] }


onUrlRequest : UrlRequest -> Msg
onUrlRequest url = NoOp


onUrlChange : Url -> Msg
onUrlChange url = NoOp


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
