module Configuration.View exposing (..)

import Configuration exposing (Configuration(..))
import Element exposing (..)
import Ident.View
import Shared
import Zone.Zone as Zone


display : Shared.Model -> Configuration -> Element msg
display s c =
    case c of
        ZoneConfig zone fs scope ->
            row [] [ text <| Zone.toDesc zone ++ " for ", Ident.View.displayScope s scope ]
