module View exposing (..)

import Browser.Navigation as Nav
import DictSet as Set
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes as Attr
import Html.Events
import Json.Decode exposing (..)
import Route exposing (Route)
import Shared
import Style exposing (..)


type alias View msg =
    { title : String
    , attributes : List (Attribute msg)
    , element : Element msg
    }


type ViewType
    = Smallcard
    | New


notFound : View msg
notFound =
    { title = "Not Found"
    , attributes = []
    , element = el [ centerX, centerY ] (text "Not Found")
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


topbar : String -> Element msg
topbar title =
    paragraph [ width fill, padding 10, Font.size size.text.topbar, Background.color color.item.topbar ] [ text title ]


topview : String -> List (Element msg) -> List (Element msg) -> Element msg
topview title buttons children =
    column [ width fill, alignTop, padding 20 ]
        [ column [ width fill, Border.shadow shadowStyle, padding 0, centerX, alignTop ]
            [ topbar title
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


goTo : Shared.Model -> Route -> Effect Shared.Msg msg
goTo s =
    Route.toString >> Nav.pushUrl s.navkey >> Effect.fromCmd


placeholder : String -> View msg
placeholder str =
    { title = str
    , attributes = []
    , element = text str
    }


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , attributes = List.map (mapAttribute fn) view.attributes
    , element = Element.map fn view.element
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
