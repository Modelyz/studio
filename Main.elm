module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, button)
import Html.Attributes exposing (src, width)
import Html.Events exposing (onClick)
import String


---- MODEL ----

type AgentType =
    AgentType
        { name: String
        , atype: AgentType
         }

type CommitmentType =
    CommitmentType
        { name: String
        , ctype: CommitmentType
         }

type EventType =
    EventType
        { name: String
        , etype: EventType
         }

type ResourceType =
    ResourceType
        { name: String
        , rtype: ResourceType
        }

type Agent =
    Agent
        { name: String
        , atype: AgentType
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
        , commitments: List Commitment
        , events: List Event
        }

type alias Pattern =
    {}

type alias Model =
    { pattern: Pattern,  instances: List Instance }


init : ( Model, Cmd Msg )
init =
    ( {pattern={}, instances=[]}, Cmd.none )



---- UPDATE ----


type Msg
    = NewSale


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSale -> ( {model | instances=model.instances++[newSale <| List.length model.instances + 1]}, Cmd.none)

newSale : Int -> Instance
newSale id =
    Instance {id=id, commitments=[], events=[]}

---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "logo.svg", width 50 ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , button [onClick NewSale] [text "New pizza sale"]
        , text <| String.join ", " <| List.map (\(Instance i) -> String.fromInt i.id) model.instances
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
