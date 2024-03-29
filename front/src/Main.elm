port module Main exposing (main)

import Agent.AddPage
import Agent.ListPage
import Agent.ViewPage
import AgentType.AddPage
import AgentType.ListPage
import AgentType.ViewPage
import Browser
import Browser.Events as Events
import Commitment.AddPage
import Commitment.ListPage
import Commitment.ViewPage
import CommitmentType.AddPage
import CommitmentType.ListPage
import CommitmentType.ViewPage
import Configuration.Zone.AddPage
import Configuration.Zone.ListPage
import Configuration.Zone.ViewPage
import Contract.AddPage
import Contract.ListPage
import Contract.ViewPage
import ContractType.AddPage
import ContractType.ListPage
import ContractType.ViewPage
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Event.AddPage
import Event.ListPage
import Event.ViewPage
import EventType.AddPage
import EventType.ListPage
import EventType.ViewPage
import Group.AddPage
import Group.ListPage
import Group.ViewPage
import GroupType.AddPage
import GroupType.ListPage
import GroupType.ViewPage
import HomePage
import IOStatus as IO
import Ident.AddPage
import Ident.ListPage
import Ident.ViewPage
import Json.Encode as Encode
import Process.AddPage
import Process.ListPage
import Process.ViewPage
import ProcessType.AddPage
import ProcessType.ListPage
import ProcessType.ViewPage
import Resource.AddPage
import Resource.ListPage
import Resource.ViewPage
import ResourceType.AddPage
import ResourceType.ListPage
import ResourceType.ViewPage
import Route
import Shared exposing (Msg(..))
import Spa exposing (mapSharedMsg)
import Value.AddPage
import Value.ListPage
import Value.ViewPage
import View exposing (View, h1)
import View.Navbar as Navbar
import View.Style as Style exposing (WindowSize, color)


port messagesReader : (Encode.Value -> msg) -> Sub msg


port messagesStored : (Encode.Value -> msg) -> Sub msg


port messagesStoredToSend : (Encode.Value -> msg) -> Sub msg


port wsSendStatus : (Encode.Value -> msg) -> Sub msg


port wsClose : (Encode.Value -> msg) -> Sub msg


port wsError : (Encode.Value -> msg) -> Sub msg


port wsOpened : (Encode.Value -> msg) -> Sub msg


port messagesReceiver : (String -> msg) -> Sub msg


port newSeed : (List Int -> msg) -> Sub msg



-- TODO get or create a session uuid, retrieve the last message time, send to haskell )


subscriptions : Shared.Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ wsSendStatus MessageSent
        , Events.onResize (\width height -> WindowResized (WindowSize width height))
        , messagesReader MessagesRead
        , messagesStoredToSend MessagesStoredTosend
        , messagesStored MessagesStored
        , messagesReceiver MessagesReceived
        , wsOpened WSConnected
        , wsClose WSDisconnected
        , wsError WSError
        , newSeed GotNewSeed
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
        [ case s.iostatus of
            IO.IOError err ->
                -- BSOD
                layout [ width fill, height fill ] <|
                    row [ width fill, height fill ]
                        [ column [ Background.color (rgb 0 0 0), Font.color (rgb 0.9 0.9 0.9), padding 20, spacing 20, width fill, height fill, Font.size 15 ]
                            [ h1 "The following critical error occured:"
                            , row [ Font.size 20 ] [ text <| "version: " ++ String.fromInt s.version ++ "\nES version: " ++ String.fromInt s.esversion ]
                            , row [ Font.family [ Font.monospace ] ] [ text err ]
                            ]
                        ]

            _ ->
                layout [ width fill, height fill ] <|
                    (if s.menu == Style.Desktop then
                        row

                     else
                        column
                    )
                        [ width fill, height fill, Font.color color.text.main ]
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
        |> Spa.addPublicPage mappers HomePage.match HomePage.page
        -- Process
        |> Spa.addPublicPage mappers ProcessType.ListPage.match ProcessType.ListPage.page
        |> Spa.addPublicPage mappers ProcessType.AddPage.match ProcessType.AddPage.page
        |> Spa.addPublicPage mappers ProcessType.ViewPage.match ProcessType.ViewPage.page
        |> Spa.addPublicPage mappers Process.AddPage.match Process.AddPage.page
        |> Spa.addPublicPage mappers Process.ViewPage.match Process.ViewPage.page
        |> Spa.addPublicPage mappers Process.ListPage.match Process.ListPage.page
        -- Resource
        |> Spa.addPublicPage mappers ResourceType.ListPage.match ResourceType.ListPage.page
        |> Spa.addPublicPage mappers ResourceType.AddPage.match ResourceType.AddPage.page
        |> Spa.addPublicPage mappers ResourceType.ViewPage.match ResourceType.ViewPage.page
        |> Spa.addPublicPage mappers Resource.AddPage.match Resource.AddPage.page
        |> Spa.addPublicPage mappers Resource.ViewPage.match Resource.ViewPage.page
        |> Spa.addPublicPage mappers Resource.ListPage.match Resource.ListPage.page
        -- Event
        |> Spa.addPublicPage mappers EventType.ListPage.match EventType.ListPage.page
        |> Spa.addPublicPage mappers EventType.AddPage.match EventType.AddPage.page
        |> Spa.addPublicPage mappers EventType.ViewPage.match EventType.ViewPage.page
        |> Spa.addPublicPage mappers Event.AddPage.match Event.AddPage.page
        |> Spa.addPublicPage mappers Event.ViewPage.match Event.ViewPage.page
        |> Spa.addPublicPage mappers Event.ListPage.match Event.ListPage.page
        -- Agent
        |> Spa.addPublicPage mappers AgentType.ViewPage.match AgentType.ViewPage.page
        |> Spa.addPublicPage mappers AgentType.ListPage.match AgentType.ListPage.page
        |> Spa.addPublicPage mappers AgentType.AddPage.match AgentType.AddPage.page
        |> Spa.addPublicPage mappers Agent.ViewPage.match Agent.ViewPage.page
        |> Spa.addPublicPage mappers Agent.ListPage.match Agent.ListPage.page
        |> Spa.addPublicPage mappers Agent.AddPage.match Agent.AddPage.page
        -- Commitment
        |> Spa.addPublicPage mappers CommitmentType.ListPage.match CommitmentType.ListPage.page
        |> Spa.addPublicPage mappers Commitment.ListPage.match Commitment.ListPage.page
        |> Spa.addPublicPage mappers CommitmentType.AddPage.match CommitmentType.AddPage.page
        |> Spa.addPublicPage mappers CommitmentType.ViewPage.match CommitmentType.ViewPage.page
        |> Spa.addPublicPage mappers Commitment.AddPage.match Commitment.AddPage.page
        |> Spa.addPublicPage mappers Commitment.ViewPage.match Commitment.ViewPage.page
        -- Contract
        |> Spa.addPublicPage mappers ContractType.ListPage.match ContractType.ListPage.page
        |> Spa.addPublicPage mappers ContractType.AddPage.match ContractType.AddPage.page
        |> Spa.addPublicPage mappers ContractType.ViewPage.match ContractType.ViewPage.page
        |> Spa.addPublicPage mappers Contract.AddPage.match Contract.AddPage.page
        |> Spa.addPublicPage mappers Contract.ViewPage.match Contract.ViewPage.page
        |> Spa.addPublicPage mappers Contract.ListPage.match Contract.ListPage.page
        -- Group
        |> Spa.addPublicPage mappers GroupType.ListPage.match GroupType.ListPage.page
        |> Spa.addPublicPage mappers GroupType.ViewPage.match GroupType.ViewPage.page
        |> Spa.addPublicPage mappers GroupType.AddPage.match GroupType.AddPage.page
        |> Spa.addPublicPage mappers Group.ViewPage.match Group.ViewPage.page
        |> Spa.addPublicPage mappers Group.AddPage.match Group.AddPage.page
        |> Spa.addPublicPage mappers Group.ListPage.match Group.ListPage.page
        -- Ident
        |> Spa.addPublicPage mappers Ident.ListPage.match Ident.ListPage.page
        |> Spa.addPublicPage mappers Ident.AddPage.match Ident.AddPage.page
        |> Spa.addPublicPage mappers Ident.ViewPage.match Ident.ViewPage.page
        |> Spa.addPublicPage mappers Configuration.Zone.ListPage.match Configuration.Zone.ListPage.page
        |> Spa.addPublicPage mappers Configuration.Zone.AddPage.match Configuration.Zone.AddPage.page
        |> Spa.addPublicPage mappers Configuration.Zone.ViewPage.match Configuration.Zone.ViewPage.page
        -- Value
        |> Spa.addPublicPage mappers Value.ListPage.match Value.ListPage.page
        |> Spa.addPublicPage mappers Value.AddPage.match Value.AddPage.page
        |> Spa.addPublicPage mappers Value.ViewPage.match Value.ViewPage.page
        |> Spa.application View.map
            { toRoute = Route.toRoute
            , init = Shared.init
            , update = Shared.update
            , subscriptions = subscriptions
            , toDocument = toDocument
            , protectPage = Route.toString
            }
        |> Browser.application
