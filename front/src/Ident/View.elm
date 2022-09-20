module Ident.View exposing (displayIdentifierDict)

-- TODO file seems unused

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Ident.Identifier as Identifier exposing (Identifier)
import View exposing (headerCell, innerCell)
import View.Style exposing (..)


displayIdentifierDict : String -> Dict String Identifier -> Element msg
displayIdentifierDict default data =
    if Dict.size data > 0 then
        table [ width shrink, Background.color color.item.background ]
            { data = Dict.values data
            , columns =
                [ { header = headerCell color.table.header.background "Identifier", width = fill, view = .name >> innerCell }
                , { header = headerCell color.table.header.background "Value", width = fill, view = Identifier.toValue >> innerCell }
                ]
            }

    else
        text default
