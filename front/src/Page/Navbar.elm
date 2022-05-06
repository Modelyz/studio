module Page.Navbar exposing (view)

import DictSet as Set
import Element as E exposing (Element, alignTop, column, fill, height, htmlAttribute, link, rgb, row, text)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes as Attr
import IOStatus as IO exposing (toText)
import Route exposing (Route(..))
import Shared
import Time exposing (posixToMillis)
import Websocket as WS exposing (toText)


view : Shared.Model -> Element msg
view s =
    column
        [ alignTop, height fill, Font.color (rgb 100 100 100), Background.color (rgb 0 0 0) ]
    <|
        [ column
            []
            [ column []
                [ link []
                    { url = "/", label = text "Home" }
                , link []
                    { url = "/process-types", label = text "Process Types" }
                , link []
                    { url = "/event-types", label = text "Event Types" }
                , link []
                    { url = "/commitment-types", label = text "Commitment Types" }
                ]
            ]
        ]
            ++ (if Set.size s.state.processTypes > 0 then
                    s.state.processTypes
                        |> Set.toList
                        |> List.map (\pt -> link [] { url = "/processes?type=" ++ pt.name, label = text pt.name })

                else
                    []
               )
            ++ [ row [ htmlAttribute <| Attr.id "WSStatus" ] [ text <| "WS=" ++ WS.toText s.wsstatus ]
               , row [ htmlAttribute <| Attr.id "IOStatus" ] [ text <| "IO=" ++ IO.toText s.iostatus ]
               , row [ htmlAttribute <| Attr.id "LastEvenTime" ] [ text <| "LastEvenTime=" ++ (String.fromInt <| posixToMillis s.state.lastEventTime) ]
               , row [ htmlAttribute <| Attr.id "timeoutReconnect" ] [ text <| "timeoutReconnect=" ++ (String.fromInt <| s.timeoutReconnect) ]
               , row [ htmlAttribute <| Attr.id "pending" ] [ text <| "pending=" ++ (String.fromInt <| Set.size s.state.pendingEvents) ]
               , row [ htmlAttribute <| Attr.id "msgs" ] [ text <| "msgs=" ++ (String.fromInt <| Set.size s.state.uuids) ]
               ]
