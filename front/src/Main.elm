port module Main exposing (main)

import Browser
import Browser.Events as Events
import Element exposing (..)
import Json.Encode as Encode
import Page.AddAgent
import Page.AddAgentType
import Page.AddCommitmentType
import Page.AddContractType
import Page.AddEventType
import Page.AddIdentifierType
import Page.AddProcessType
import Page.AddResourceType
import Page.AgentTypes
import Page.Agents
import Page.CommitmentTypes
import Page.ContractTypes
import Page.EventTypes
import Page.Groups
import Page.Home
import Page.IdentifierTypes
import Page.Process
import Page.ProcessType
import Page.ProcessTypes
import Page.Processes
import Page.ResourceTypes
import Route exposing (toRoute)
import Shared exposing (Msg(..))
import Spa exposing (mapSharedMsg)
import Style exposing (WindowSize)
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
        , Events.onResize (\width height -> WindowResized (WindowSize width height))
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
        [ layout [ width fill, height fill ] <|
            (if s.menu == Style.Desktop then
                row

             else
                column
            )
                [ width fill, height fill ]
                [ Element.map mapSharedMsg (Navbar.view view.title s view.route)
                , view.element s
                ]
        ]
    }


main =
    Spa.init
        { defaultView = View.notFound
        , extractIdentity = Shared.identity
        }
        |> Spa.addPublicPage mappers Page.Home.match Page.Home.page
        |> Spa.addPublicPage mappers Page.ProcessTypes.match Page.ProcessTypes.page
        |> Spa.addPublicPage mappers Page.ResourceTypes.match Page.ResourceTypes.page
        |> Spa.addPublicPage mappers Page.EventTypes.match Page.EventTypes.page
        |> Spa.addPublicPage mappers Page.AgentTypes.match Page.AgentTypes.page
        |> Spa.addPublicPage mappers Page.CommitmentTypes.match Page.CommitmentTypes.page
        |> Spa.addPublicPage mappers Page.ContractTypes.match Page.ContractTypes.page
        |> Spa.addPublicPage mappers Page.AddContractType.match Page.AddContractType.page
        |> Spa.addPublicPage mappers Page.ProcessType.match Page.ProcessType.page
        |> Spa.addPublicPage mappers Page.Processes.match Page.Processes.page
        |> Spa.addPublicPage mappers Page.Process.match Page.Process.page
        |> Spa.addPublicPage mappers Page.Groups.match Page.Groups.page
        |> Spa.addPublicPage mappers Page.IdentifierTypes.match Page.IdentifierTypes.page
        |> Spa.addPublicPage mappers Page.AddIdentifierType.match Page.AddIdentifierType.page
        |> Spa.addPublicPage mappers Page.AddResourceType.match Page.AddResourceType.page
        |> Spa.addPublicPage mappers Page.AddEventType.match Page.AddEventType.page
        |> Spa.addPublicPage mappers Page.AddProcessType.match Page.AddProcessType.page
        |> Spa.addPublicPage mappers Page.AddCommitmentType.match Page.AddCommitmentType.page
        |> Spa.addPublicPage mappers Page.AddAgentType.match Page.AddAgentType.page
        |> Spa.addPublicPage mappers Page.Agents.match Page.Agents.page
        |> Spa.addPublicPage mappers Page.AddAgent.match Page.AddAgent.page
        |> Spa.application View.map
            { toRoute = Route.toRoute
            , init = Shared.init
            , update = Shared.update
            , subscriptions = subscriptions
            , toDocument = toDocument
            , protectPage = Route.toString
            }
        |> Browser.application
