module View.Style exposing (Menu(..), WindowSize, color, darken, isMobile, itemHoverstyle, navbarHoverstyle, shadowStyle, size, strcolor)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border


type alias WindowSize =
    { w : Int
    , h : Int
    }


type Menu
    = Desktop
    | MobileClosed
    | MobileOpen


isMobile : WindowSize -> Bool
isMobile window =
    window.w < 992


size : { text : { xl : number, large : number, medium : number, main : number, small : number, h1 : number, h2 : number, h3 : number, topbar : number } }
size =
    { text =
        { xl = 50
        , large = 30
        , medium = 20
        , main = 15
        , small = 12
        , h1 = 28
        , h2 = 24
        , h3 = 20
        , topbar = 20
        }
    }


strcolor : String
strcolor =
    "#c5e8f7"


color =
    -- TODO remove unused colors
    { navbar =
        { background = rgb255 0xC5 0xE8 0xF7
        , separator = rgb255 0xE5 0xF8 0xFF
        , text = rgb255 0x00 0x00 0x00
        , hover = rgb255 0xE5 0xF8 0xFF
        }
    , topbar =
        { background = rgb255 0xC5 0xE8 0xF7
        , disabled = rgb255 0xD0 0xD0 0xD0
        , border = rgb255 0xFF 0xFF 0xFF
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
    , table =
        { header =
            { background = rgb255 0xC5 0xE8 0xF7
            , background2 = rgb255 0xF7 0xD2 0xC5
            , text = rgb255 0x00 0x00 0x00
            , background3 = rgb255 0xFF 0x97 0x00
            }
        , inner =
            { background = rgb255 0xE0 0xE0 0xE0
            , text = rgb255 0x00 0x00 0x00
            }
        }
    , border =
        { main = rgb255 0x72 0x9F 0xCF
        , focus = rgb255 0xE0 0xE0 0xE0
        }
    , text =
        { main = rgb255 0x20 0x20 0x20
        , item = rgb255 0xE0 0xE0 0xE0
        , light = rgb255 0xA0 0xA0 0xA0
        , disabled = rgb255 0xD0 0xD0 0xD0
        , warning = rgb255 0xFF 0x97 0x00
        , error = rgb255 0xFF 0x00 0x54
        }
    , button =
        { primary = rgb255 0xC5 0xE8 0xF7
        , secondary = rgb255 0xE0 0xE0 0xE0
        , special = rgb255 0xF7 0xDC 0xC5
        , disabled = rgb255 0xF0 0xF0 0xF0
        }
    , content =
        { separator = rgb255 0xE0 0xE0 0xE0
        , background = rgb255 0xFF 0xFF 0xFF
        , choice = rgb255 0xF9 0xF9 0xF9
        }
    }


darken : Float -> Color -> Color
darken coef =
    toRgb >> (\{ red, green, blue, alpha } -> rgba (red * coef) (green * coef) (blue * coef) alpha)


shadowStyle : { offset : ( number, number ), size : number, blur : number, color : Color }
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
