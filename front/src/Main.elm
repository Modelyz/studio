port module Main exposing (main)

import Browser
import Element exposing (..)
import Event exposing (Event(..))
import EventFlow exposing (EventFlow(..))
import IOStatus exposing (IOStatus(..))
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Page.CommitmentTypes
import Page.EventTypes
import Page.Groups
import Page.Home
import Page.Identifiers
import Page.Process
import Page.ProcessType
import Page.ProcessTypes
import Page.Processes
import Route exposing (toRoute)
import Shared exposing (Msg(..))
import Spa
import View exposing (View)
import View.Navbar as Navbar



-- TODO gather ports in a single typed one


port eventsReader : (Encode.Value -> msg) -> Sub msg


port eventsStored : (Encode.Value -> msg) -> Sub msg


port eventsStoredToSend : (Encode.Value -> msg) -> Sub msg


port wsSendStatus : (Encode.Value -> msg) -> Sub msg


port wsClose : (Encode.Value -> msg) -> Sub msg


port wsError : (Encode.Value -> msg) -> Sub msg


port wsOpened : (Encode.Value -> msg) -> Sub msg


port eventsReceiver : (String -> msg) -> Sub msg



-- TODO get or create a session uuid, retrieve the last event time, send to haskell )


subscriptions : Shared.Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ wsSendStatus EventsSent
        , eventsReader EventsRead
        , eventsStoredToSend EventsStoredTosend
        , eventsStored EventsStored
        , eventsReceiver EventsReceived
        , wsOpened WSConnected
        , wsClose WSDisconnected
        , wsError WSError
        ]


mappers : ( (a -> b) -> View a -> View b, (c -> d) -> View c -> View d )
mappers =
    ( View.map, View.map )


toDocument :
    Shared.Model
    -> View (Spa.Msg Shared.Msg pageMsg)
    -> Browser.Document (Spa.Msg Shared.Msg pageMsg)
toDocument s view =
    { title = view.title
    , body =
        [ layout [ width fill, height fill ] view.element
        ]
    }


main =
    Spa.init
        { defaultView = View.notFound
        , extractIdentity = Shared.identity
        }
        |> Spa.addPublicPage mappers Page.Home.match Page.Home.page
        |> Spa.addPublicPage mappers Page.CommitmentTypes.match Page.CommitmentTypes.page
        |> Spa.addPublicPage mappers Page.EventTypes.match Page.EventTypes.page
        |> Spa.addPublicPage mappers Page.ProcessTypes.match Page.ProcessTypes.page
        |> Spa.addPublicPage mappers Page.ProcessType.match Page.ProcessType.page
        |> Spa.addPublicPage mappers Page.Processes.match Page.Processes.page
        |> Spa.addPublicPage mappers Page.Process.match Page.Process.page
        |> Spa.addPublicPage mappers Page.Groups.match Page.Groups.page
        |> Spa.addPublicPage mappers Page.Identifiers.match Page.Identifiers.page
        |> Spa.application View.map
            { toRoute = Route.toRoute
            , init = Shared.init
            , update = Shared.update
            , subscriptions = subscriptions
            , toDocument = toDocument
            , protectPage = Route.toString
            }
        |> Browser.application
