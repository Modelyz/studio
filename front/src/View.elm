module View exposing (View, button, color, h1, h2, h3, map, onEnter, p, shadow, size)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Events
import Json.Decode exposing (..)


type alias View msg =
    { title : String
    , attributes : List (Attribute msg)
    , element : Element msg
    }


h1 : String -> Element msg
h1 title =
    paragraph [ Font.size size.text.h1, Region.heading 1 ] [ text title ]


h2 : String -> Element msg
h2 title =
    paragraph [ Font.size size.text.h1, Region.heading 2 ] [ text title ]


h3 : String -> Element msg
h3 title =
    paragraph [ Font.size size.text.h1, Region.heading 3 ] [ text title ]


p : String -> Element msg
p content =
    paragraph [ Font.size size.text.main ] [ text content ]


size =
    { text = { main = 15, small = 12, h1 = 35, h2 = 25, h3 = 17 } }


color =
    { background =
        { main = rgb255 0xFF 0xFF 0xFF
        , alt = rgb255 0x2E 0x34 0x36
        , alt2 = rgb255 0xEF 0xEF 0xEF
        , focus = rgb255 0xE0 0xE0 0xE0
        , selected = rgb255 0xC5 0xE8 0xF7
        , warning = rgb255 0xFF 0x97 0x00
        , error = rgb255 0xFF 0x00 0x54
        }
    , border =
        { main = rgb255 0x72 0x9F 0xCF
        , focused = rgb255 0xE0 0xE0 0xE0
        , selected = rgb255 0xC5 0xE8 0xF7
        }
    , text =
        { main = rgb255 0xE0 0xE0 0xE0
        , alt = rgb255 0xE0 0xE0 0xE0
        , focus = rgb255 0xE0 0xE0 0xE0
        , warning = rgb255 0xFF 0x97 0x00
        , error = rgb255 0xFF 0x00 0x54
        }
    , button =
        { primary = rgb255 0xC5 0xE8 0xF7
        , secondary = rgb255 0xE0 0xE0 0xE0
        }
    }


button =
    { primary = Input.button [ Background.color color.button.primary, padding 10 ]
    , secondary = Input.button [ Background.color color.button.secondary, padding 10 ]
    }


shadow =
    Border.shadow { offset = ( 3, 3 ), size = 4, blur = 9, color = color.border.focused }


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
