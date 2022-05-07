module View exposing (View, color, map, onEnter)

import Element exposing (..)
import Html.Events
import Json.Decode exposing (..)


type alias View msg =
    { title : String
    , attributes : List (Attribute msg)
    , element : Element msg
    }


color =
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , white = rgb255 0xFF 0xFF 0xFF
    }


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
