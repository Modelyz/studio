module View exposing (View, button, closeMenu, flatContainer, floatingContainer, floatingContainer2, h1, h2, h3, hamburger, headerCell, innerCell, lenToPx, map, notFound, onEnter, p, separator, switch, viewSelector, withDefaultContent, zero)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import Route exposing (Route)
import Shared
import Util exposing (flip)
import View.Style as Style exposing (..)
import View.Type as ViewType


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


hamburger : String -> Element Shared.Msg
hamburger title =
    row []
        [ column
            [ width (px 40)
            , alignTop
            , spacing 5
            , Background.color color.navbar.background
            , paddingEach { left = 10, top = 10, right = 50, bottom = 10 }
            , onClick Shared.ToggleMenu
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


topbar : Shared.Model -> Maybe msg -> String -> Color -> Element msg
topbar s goBack title background =
    if s.menu == Style.Desktop then
        row
            [ Border.widthEach { zero | left = 1 }
            , Border.color color.topbar.border
            , width fill
            , height (px 42)
            , Font.size size.text.topbar
            , Background.color background
            ]
            [ Maybe.map (\m -> button.primary m " < ") goBack |> Maybe.withDefault none
            , el [ padding 10 ] <| text title
            ]

    else
        none


floatingContainer : Shared.Model -> Maybe msg -> String -> List (Element msg) -> List (Element msg) -> Element msg
floatingContainer s goBack title buttons children =
    column [ width fill, alignTop, padding 20 ]
        [ column [ width fill, Border.shadow shadowStyle, padding 0, centerX, alignTop ]
            [ topbar s goBack title color.topbar.background
            , column [ width fill, padding 20, centerX, alignTop, spacing 20, Background.color color.content.background ]
                (column
                    [ width fill, alignTop ]
                    [ wrappedRow [ width fill, spacing 20 ]
                        buttons
                    ]
                    :: children
                )
            ]
        ]


floatingContainer2 : Shared.Model -> Maybe msg -> String -> List (Element msg) -> List (Element msg) -> Maybe (Element msg) -> Element msg
floatingContainer2 s goBack title buttons children subpage =
    -- TODO merge with floatingContainer
    column [ width fill, alignTop, padding 20 ]
        [ column [ width fill, Border.shadow shadowStyle, padding 0, centerX, alignTop, inFront (subpage |> Maybe.withDefault none) ]
            [ topbar s goBack title (subpage |> Maybe.map (\_ -> color.topbar.disabled) |> Maybe.withDefault color.topbar.background)
            , column [ width fill, padding 20, centerX, alignTop, spacing 20, Background.color color.content.background ]
                (column
                    [ width fill, alignTop ]
                    [ wrappedRow [ width fill, spacing 20 ]
                        buttons
                    ]
                    :: children
                )
            ]
        ]


flatContainer : Shared.Model -> Maybe msg -> String -> List (Element msg) -> Element msg -> Element msg -> List (Element msg) -> Element msg
flatContainer s goBack title buttons search viewselector children =
    -- container for main content
    column [ width fill, alignTop, padding 0 ]
        [ topbar s goBack title color.topbar.background
        , column [ width fill, padding 20, centerX, alignTop, spacing 20, Background.color color.content.background ]
            [ column
                [ width fill, alignTop, spacing 20, padding 20 ]
                (wrappedRow [ spacing 20 ] (buttons ++ [ viewselector, search ]) :: children)
            ]
        ]


withDefaultContent : Element msg -> List (Element msg) -> List (Element msg)
withDefaultContent e xs =
    if List.isEmpty xs then
        [ e ]

    else
        xs


button :
    { primary : msg -> String -> Element msg
    , secondary : a -> String -> Element a
    , special : b -> String -> Element b
    , disabled : String -> String -> Element c
    }
button =
    { primary =
        \msg txt ->
            Input.button [ mouseOver [ Background.color color.button.prim_hover ], Background.color color.button.primary, padding 10, height fill ] { onPress = Just msg, label = text txt }
    , secondary =
        \msg txt ->
            Input.button [ mouseOver [ Background.color color.button.sec_hover ], Background.color color.button.secondary, padding 10, height fill ] { onPress = Just msg, label = text txt }
    , special =
        \msg txt ->
            Input.button [ mouseOver [ Background.color color.button.spec_hover ], Background.color color.button.special, padding 10, height fill ] { onPress = Just msg, label = text txt }
    , disabled =
        \err txt ->
            row [ htmlAttribute <| Attr.title err, spacing 20 ] [ Input.button [ Background.color color.button.disabled, Font.color color.text.disabled, padding 10, height fill ] { onPress = Nothing, label = text txt } ]
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
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


headerCell : Color -> String -> Element msg
headerCell c =
    text >> el [ padding 5, Border.width 2, Border.color color.content.background, Background.color c ]


innerCell : String -> Element msg
innerCell =
    text >> el [ height fill, padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ]


viewSelector : List ViewType.Type -> ViewType.Type -> (ViewType.Type -> msg) -> Element msg
viewSelector all selected change =
    -- display the view type selector
    row [] <|
        List.map
            (\t ->
                if t == selected then
                    button.primary (change t) (ViewType.toString t)

                else
                    button.secondary (change t) (ViewType.toString t)
            )
            all


zero : { top : number, right : number, bottom : number, left : number }
zero =
    { top = 0, right = 0, bottom = 0, left = 0 }


bound : Int -> Int -> Int -> Int
bound m mm x =
    min mm (max m x)


lenToPx : String -> String
lenToPx str =
    bound 14 30 (35 - String.length str) |> String.fromInt |> flip (++) "px"


switch : (Bool -> msg) -> Bool -> String -> String -> Element msg
switch onSwitch b false true =
    row [ Background.color color.item.selected, pointer, spacing 10, Font.size size.text.main, padding 10 ]
        [ el [ onClick (onSwitch False) ] (text false)
        , row [ onClick <| onSwitch (not b), Background.color color.content.background, width (px 48), height (px 24), Border.color color.content.choice, Border.width 2, Border.rounded 12 ]
            [ row
                [ Background.color color.item.selected
                , width (px 24)
                , height fill
                , if b then
                    alignRight

                  else
                    alignLeft
                , Border.rounded 12
                ]
                []
            ]
        , el [ onClick (onSwitch True) ] (text true)
        ]
