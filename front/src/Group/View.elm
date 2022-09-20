module Group.View exposing (displayGroupTable)

import Element exposing (..)
import Element.Background as Background
import View exposing (headerCell, innerCell)
import View.Style exposing (..)


displayGroupTable : String -> List String -> Element msg
displayGroupTable default groups =
    if List.length groups > 0 then
        table [ width shrink, Background.color color.item.background ]
            { data = groups
            , columns =
                [ { header = headerCell color.table.header.background2 "Groups", width = fill, view = innerCell }
                ]
            }

    else
        text default
