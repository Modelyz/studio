module View exposing (..)

import DictSet as Set
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes as Attr
import Html.Events
import Json.Decode exposing (..)
import Shared


type alias View msg =
    { title : String
    , attributes : List (Attribute msg)
    , element : Element msg
    }


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


size =
    { text = { main = 15, small = 12, h1 = 35, h2 = 25, h3 = 20 } }


color =
    -- TODO remove unused colors
    { navbar =
        { background = rgb255 0xC5 0xE8 0xF7
        , separator = rgb255 0xE5 0xF8 0xFF
        , text = rgb255 0x00 0x00 0x00
        , hover = rgb255 0xE5 0xF8 0xFF
        }
    , item =
        { background = rgb255 0xF0 0xF0 0xF0
        , border = rgb255 0xF0 0xF0 0xF0
        , selected = rgb255 0xC5 0xE8 0xF7
        , warning = rgb255 0xFF 0x97 0x00
        , error = rgb255 0xFF 0x00 0x54
        }
    , widget =
        { background = rgb255 0xFF 0xFF 0xFF
        , selected = rgb255 0xC5 0xE8 0xF7
        }
    , border =
        { main = rgb255 0x72 0x9F 0xCF
        , focus = rgb255 0xE0 0xE0 0xE0
        }
    , text =
        { main = rgb255 0x00 0x00 0x00
        , item = rgb255 0xE0 0xE0 0xE0
        , warning = rgb255 0xFF 0x97 0x00
        , error = rgb255 0xFF 0x00 0x54
        }
    , button =
        { primary = rgb255 0xC5 0xE8 0xF7
        , secondary = rgb255 0xE0 0xE0 0xE0
        , prim_hover = rgb255 0xB5 0xD8 0xE7
        , sec_hover = rgb255 0xD0 0xD0 0xD0
        }
    , content = { separator = rgb255 0xE0 0xE0 0xE0 }
    }


button =
    { primary = Input.button [ mouseOver [ Background.color color.button.prim_hover ], Background.color color.button.primary, padding 10 ]
    , secondary = Input.button [ mouseOver [ Background.color color.button.sec_hover ], Background.color color.button.secondary, padding 10 ]
    }


shadowStyle =
    { offset = ( 2, 2 ), size = 3, blur = 5, color = color.border.focus }


navbarHoverstyle : List Decoration
navbarHoverstyle =
    [ Background.color color.navbar.hover
    ]


itemHoverstyle : List Decoration
itemHoverstyle =
    [ Border.color color.border.focus
    , Background.color color.item.background
    , Border.shadow shadowStyle
    ]


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
                [ el [ padding 10 ] (el [ Font.size size.text.main ] <| text title)
                , button.primary
                    { onPress = Just msg, label = text "×" }
                ]
            , if description == "" then
                none

              else
                row [ padding 10, Font.size size.text.small ] [ text description ]
            ]
        ]
