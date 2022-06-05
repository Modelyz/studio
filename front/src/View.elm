module View exposing (..)

import Browser.Navigation as Nav
import DictSet as Set
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes as Attr
import Html.Events
import Json.Decode exposing (..)
import Route exposing (Route)
import Shared
import Spa exposing (mapSharedMsg)
import Style exposing (..)


type alias View msg =
    { title : String
    , attributes : List (Attribute msg)
    , element : Shared.Model -> Element msg
    , route : Route
    }


type ViewType
    = Smallcard
    | New


notFound : View msg
notFound =
    { title = "Not Found"
    , attributes = []
    , element = \_ -> el [ centerX, centerY ] (text "Not Found")
    , route = Route.Home
    }


h1 : String -> Element msg
h1 title =
    paragraph [ Font.size size.text.h1, Region.heading 1 ] [ text title ]


h2 : String -> Element msg
h2 title =
    paragraph [ Font.size size.text.h2, Region.heading 2 ] [ text title ]


h3 : String -> Element msg
h3 title =
    paragraph [ Font.size size.text.h3, Region.heading 3 ] [ text title ]


p : String -> Element msg
p content =
    paragraph [ Font.size size.text.main ] [ text content ]


separator : Color -> Element msg
separator c =
    row [ width fill, Border.width 1, Border.color c ] []


hamburger : String -> Shared.Model -> Element Shared.Msg
hamburger title s =
    let
        msg =
            if s.menu == Style.Desktop then
                Shared.None ()

            else
                Shared.ToggleMenu
    in
    row []
        [ column
            [ width (px 40)
            , alignTop
            , spacing 5
            , Background.color color.navbar.background
            , paddingEach { left = 10, top = 10, right = 50, bottom = 10 }
            , Events.onClick Shared.ToggleMenu
            , pointer
            ]
            (List.repeat 3 <|
                row
                    [ width (px 30)
                    , height (px 4)
                    , Font.color color.navbar.text
                    , Background.color color.navbar.text
                    , Border.rounded 2
                    ]
                    []
            )
        , text title
        ]


topbar : Shared.Model -> String -> Element msg
topbar s title =
    if s.menu == Style.Desktop then
        row
            [ Border.widthEach { bottom = 0, left = 1, right = 0, top = 0 }
            , Border.color color.topbar.border
            , width fill
            , padding 10
            , height (px 42)
            , Font.size size.text.topbar
            , Background.color color.topbar.background
            ]
            [ text title
            ]

    else
        none


cardContent : Shared.Model -> String -> List (Element msg) -> List (Element msg) -> Element msg
cardContent s title buttons children =
    column [ width fill, alignTop, padding 20 ]
        [ column [ width fill, Border.shadow shadowStyle, padding 0, centerX, alignTop ]
            [ topbar s title
            , column [ width fill, padding 20, centerX, alignTop, spacing 20 ]
                [ column
                    [ width fill, alignTop, spacing 20, padding 20 ]
                    (buttons ++ children)
                ]
            ]
        ]


flatContent : Shared.Model -> String -> List (Element msg) -> List (Element msg) -> Element msg
flatContent s title buttons children =
    column [ width fill, alignTop, padding 0 ]
        [ column [ width fill, padding 0, centerX, alignTop ]
            [ topbar s title
            , column [ width fill, padding 20, centerX, alignTop, spacing 20 ]
                [ column
                    [ width fill, alignTop, spacing 20, padding 20 ]
                    (buttons ++ children)
                ]
            ]
        ]


withDefaultContent : Element msg -> List (Element msg) -> List (Element msg)
withDefaultContent e xs =
    if List.isEmpty xs then
        [ e ]

    else
        xs


button =
    { primary =
        \msg txt ->
            Input.button
                [ mouseOver [ Background.color color.button.prim_hover ], Background.color color.button.primary, padding 10 ]
                { onPress = Just msg, label = text txt }
    , secondary = \msg txt -> Input.button [ mouseOver [ Background.color color.button.sec_hover ], Background.color color.button.secondary, padding 10 ] { onPress = Just msg, label = text txt }
    , disabled =
        \err txt ->
            row [ htmlAttribute <| Attr.title err, spacing 20 ]
                [ Input.button [ Background.color color.button.disabled, Font.color color.text.disabled, padding 10 ] { onPress = Nothing, label = text txt } ]
    }


redirect : Nav.Key -> Route -> Effect Shared.Msg msg
redirect navkey =
    -- redirect to the specified route
    Route.toString >> Nav.pushUrl navkey >> Effect.fromCmd


redirectParent : Nav.Key -> Route -> Effect Shared.Msg msg
redirectParent navkey route =
    -- redirect to the parent route of the specified (one level up the path)
    ("/" ++ Route.firstSegment route) |> Nav.pushUrl navkey |> Effect.fromCmd


closeMenu : Shared.Model -> Effect Shared.Msg msg
closeMenu s =
    if s.menu == MobileOpen then
        Effect.fromShared Shared.ToggleMenu

    else
        Effect.none


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , attributes = List.map (mapAttribute fn) view.attributes
    , element = \s -> Element.map fn <| view.element s
    , route = view.route
    }


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (field "key" string
                |> andThen
                    (\key ->
                        if key == "Enter" then
                            succeed msg

                        else
                            fail "Not the enter key"
                    )
            )
        )


viewSmallCard : msg -> String -> String -> Element msg
viewSmallCard msg title description =
    row
        [ htmlAttribute <| Attr.id title ]
        [ column [ Background.color color.item.background ]
            [ row [ spacing 10, width fill ]
                [ row [ Font.size size.text.main, padding 10 ] [ text title ]
                , el [ alignRight ] (button.primary msg "×")
                ]
            , if description == "" then
                none

              else
                row [ padding 10, Font.size size.text.small ] [ text description ]
            ]
        ]


checkEmptyString : String -> String -> Result String String
checkEmptyString field err =
    if String.isEmpty field then
        Err err

    else
        Ok field


checkEmptyList : List a -> String -> Result String (List a)
checkEmptyList field err =
    if List.isEmpty field then
        Err err

    else
        Ok field


checkNothing : Maybe a -> String -> Result String a
checkNothing field err =
    case field of
        Nothing ->
            Err err

        Just x ->
            Ok x


clickableCard : msg -> String -> Maybe String -> Element msg
clickableCard onInput title desc =
    column [ pointer, onClick onInput, Background.color color.item.background, mouseOver itemHoverstyle, width (px 150), height (px 75) ]
        [ row [ alignLeft ]
            [ button.primary onInput "+"
            , el [ paddingXY 10 0 ] (text title)
            ]
        , desc |> Maybe.map (\d -> paragraph [ padding 10, Font.size size.text.main ] [ text d ]) |> Maybe.withDefault none
        ]
