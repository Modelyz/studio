module View exposing (View, defaultView, map)

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes


type alias View msg =
    { title : String
    , attributes : List (Attribute msg)
    , element : Html msg
    }


defaultView : View msg
defaultView =
    placeholder "404"


placeholder : String -> View msg
placeholder str =
    { title = str
    , attributes = []
    , element = Html.text str
    }


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , attributes = List.map (Attributes.map fn) view.attributes
    , element = Html.map fn view.element
    }
