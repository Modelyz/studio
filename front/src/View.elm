module View exposing (..)

import DictSet as Set exposing (DictSet)
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
import View.Style as Style exposing (..)


type alias View msg =
    { title : String
    , attributes : List (Attribute msg)
    , element : Shared.Model -> Element msg
    , route : Route
    }


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


floatingContainer : Shared.Model -> String -> List (Element msg) -> List (Element msg) -> Element msg
floatingContainer s title buttons children =
    column [ width fill, alignTop, padding 20 ]
        [ column [ width fill, Border.shadow shadowStyle, padding 0, centerX, alignTop ]
            [ topbar s title
            , column [ width fill, padding 20, centerX, alignTop, spacing 20 ]
                ([ column
                    [ width fill, alignTop ]
                    [ wrappedRow [ width fill, spacing 20 ]
                        buttons
                    ]
                 ]
                    ++ children
                )
            ]
        ]


flatContainer : Shared.Model -> String -> List (Element msg) -> Element msg -> List (Element msg) -> Element msg
flatContainer s title buttons search children =
    -- container for main content
    column [ width fill, alignTop, padding 0 ]
        [ topbar s title
        , column [ width fill, padding 20, centerX, alignTop, spacing 20 ]
            [ column
                [ width fill, alignTop, spacing 20, padding 20 ]
                (buttons ++ [ search ] ++ children)
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


closeMenu : { flags | route : Route } -> Style.Menu -> Effect Shared.Msg msg
closeMenu f menu =
    Effect.batch
        -- store the route to reload page init to the same route after reading messages
        -- on the first run the entityType is Nothing, and it is found at the second
        [ Effect.fromShared (Shared.SetRoute f.route)
        , if menu == MobileOpen then
            Effect.fromShared Shared.ToggleMenu

          else
            Effect.none
        ]


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


checkEmptyString : String -> String -> Result String String
checkEmptyString str err =
    if String.isEmpty str then
        Err err

    else
        Ok str


checkEmptyList : List a -> String -> Result String (List a)
checkEmptyList list err =
    if List.isEmpty list then
        Err err

    else
        Ok list


checkEmptyDict : DictSet comparable a -> String -> Result String (DictSet comparable a)
checkEmptyDict field err =
    if Set.isEmpty field then
        Err err

    else
        Ok field


checkNothing : Maybe a -> String -> Result String (Maybe a)
checkNothing field err =
    case field of
        Nothing ->
            Err err

        Just x ->
            Ok (Just x)


clickableCard : msg -> Element msg -> Element msg -> Element msg
clickableCard onInput title desc =
    -- TODO choose a more explicit name
    column [ pointer, onClick onInput, Background.color color.item.background, mouseOver itemHoverstyle, height (px 75) ]
        [ row [ alignLeft, width <| minimum 150 shrink ]
            [ button.primary onInput "+"
            , el [ paddingXY 10 0 ] title
            ]
        , desc
        ]
