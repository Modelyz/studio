module View.Navbar exposing (view)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes as Attr
import IOStatus as IO
import Prng.Uuid as Uuid
import Route exposing (Route, toString)
import Shared
import Type exposing (Type(..))
import View exposing (zero)
import View.Style as Style exposing (color, navbarHoverstyle)
import Websocket as WS


view : String -> Shared.Model -> Route -> Element Shared.Msg
view title s =
    if s.menu == Style.Desktop then
        desktop s

    else
        mobile title s


links : Shared.Model -> Route -> List (Element Shared.Msg)
links s r =
    if s.admin then
        adminLinks s r

    else
        userLinks s r


adminLinks : Shared.Model -> Route -> List (Element Shared.Msg)
adminLinks s r =
    menuitem s r Route.Home
        :: List.map
            (\e -> menuitem s r (Route.Entity e (Route.List { type_ = Nothing })))
            Route.allTypes
        ++ List.map
            (\e -> menuitem s r (Route.Entity e (Route.List { type_ = Nothing })))
            Route.allBehaviours


userLinks : Shared.Model -> Route -> List (Element Shared.Msg)
userLinks s r =
    menuitem s r Route.Home
        :: ((s.state.resourceTypes
                |> Dict.values
                |> List.filter (Route.isMenu s.state)
                |> List.map (\e -> menuitem s r (Route.Entity Route.Resource (Route.List { type_ = Just (Uuid.toString e.uuid) })))
            )
                ++ (s.state.eventTypes
                        |> Dict.values
                        |> List.filter (Route.isMenu s.state)
                        |> List.map (\e -> menuitem s r (Route.Entity Route.Event (Route.List { type_ = Just (Uuid.toString e.uuid) })))
                   )
                ++ (s.state.agentTypes
                        |> Dict.values
                        |> List.filter (Route.isMenu s.state)
                        |> List.map (\e -> menuitem s r (Route.Entity Route.Agent (Route.List { type_ = Just (Uuid.toString e.uuid) })))
                   )
                ++ (s.state.commitmentTypes
                        |> Dict.values
                        |> List.filter (Route.isMenu s.state)
                        |> List.map (\e -> menuitem s r (Route.Entity Route.Commitment (Route.List { type_ = Just (Uuid.toString e.uuid) })))
                   )
                ++ (s.state.contractTypes
                        |> Dict.values
                        |> List.filter (Route.isMenu s.state)
                        |> List.map (\e -> menuitem s r (Route.Entity Route.Contract (Route.List { type_ = Just (Uuid.toString e.uuid) })))
                   )
                ++ (s.state.processTypes
                        |> Dict.values
                        |> List.filter (Route.isMenu s.state)
                        |> List.map (\e -> menuitem s r (Route.Entity Route.Process (Route.List { type_ = Just (Uuid.toString e.uuid) })))
                   )
                ++ (s.state.groupTypes
                        |> Dict.values
                        |> List.filter (Route.isMenu s.state)
                        |> List.map (\e -> menuitem s r (Route.Entity Route.Group (Route.List { type_ = Just (Uuid.toString e.uuid) })))
                   )
                |> View.withDefaultContent
                    (row
                        [ Font.size 15, spacing 10, padding 10, width fill, mouseOver navbarHoverstyle, Background.color color.navbar.hover ]
                        [ el [ Background.color color.content.background, width (px 15), alignLeft ] (text " ")
                        , View.p "(no configuration yet)"
                        ]
                    )
           )


mobile : String -> Shared.Model -> Route -> Element Shared.Msg
mobile title s r =
    column [ width fill, padding 0, Font.color color.navbar.text, Background.color color.navbar.background ] <|
        List.intersperse
            (View.separator color.navbar.separator)
        <|
            View.hamburger title
                :: (if s.menu == Style.MobileClosed then
                        []

                    else
                        links s r
                   )


desktop : Shared.Model -> Route -> Element Shared.Msg
desktop s r =
    column
        [ width (px 250), padding 10, alignTop, height fill, Font.color color.navbar.text, Background.color color.navbar.background, clip ]
    <|
        List.intersperse (View.separator color.navbar.separator) (links s r)
            ++ [ column [ width fill, spacing 5, alignBottom ]
                    [ row [ centerX, Font.color color.navbar.text, padding 10, Font.size 15, Font.bold ] [ text "Studio ", newTabLink [ Font.regular ] { url = "/changelog", label = text <| "(version " ++ String.fromInt s.version ++ ")" } ]
                    , View.separator color.navbar.separator
                    , row [ width fill ]
                        [ column [ width fill, paddingXY 5 0 ]
                            [ el [ paddingXY 0 5, htmlAttribute <| Attr.title <| WS.toText s.wsstatus ] (text <| WS.toEmoji s.wsstatus ++ " WS")
                            , el [ htmlAttribute <| Attr.title <| IO.toText s.iostatus ] (text <| IO.toEmoji s.iostatus ++ " IO")
                            ]
                        , column [ Border.color color.navbar.separator, Border.widthEach { zero | left = 1 }, width fill, centerX, centerY ]
                            [ View.switch Shared.SwitchAdmin s.admin "user" "admin"
                            , View.switch Shared.SwitchOffline s.offline "online" "offline"
                            ]
                        ]
                    ]
               ]


menuitem : Shared.Model -> Route -> Route -> Element msg
menuitem s currentRoute route =
    let
        active =
            case currentRoute of
                Route.Entity e1 v1 ->
                    case route of
                        Route.Entity e2 v2 ->
                            e1 == e2 && Route.toTypeFilter v1 == Route.toTypeFilter v2

                        _ ->
                            False

                _ ->
                    False
    in
    row
        ([ Font.size 15, spacing 10, padding 10, width fill, mouseOver navbarHoverstyle ]
            ++ (if active then
                    [ Background.color color.navbar.hover ]

                else
                    []
               )
        )
        [ el [ Background.color (Route.toColor route), width (px 15), alignLeft ] (text " ")
        , link
            ([]
                ++ (if active then
                        [ Background.color color.navbar.hover ]

                    else
                        []
                   )
            )
            { url = toString route, label = text <| Route.toDesc s.state route }
        ]
