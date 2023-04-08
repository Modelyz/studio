module Group.View exposing (displayGroupTable, groupsColumn)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Group.Group exposing (groupsOf)
import Prng.Uuid exposing (Uuid)
import Shared
import Type exposing (Type)
import Typed.Type as TType
import View exposing (headerCell, innerCell)
import View.Style exposing (..)
import Configuration.Zone.View exposing (displayZone)
import Configuration.Zone exposing (Zone(..))


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


groupsColumn : Shared.Model -> Column ( Uuid, Type, Maybe Uuid ) msg
groupsColumn s =
    { header = headerCell color.table.header.background2 "Groups"
    , width = fill
    , view =
        \( uuid, _, _ ) ->
            groupsOf s.state.grouped uuid
                |> List.map (displayZone s.state SmallcardTitle (Type.TType TType.Group))
                |> String.join "\n"
                |> text
                |> el [ height fill, padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ]
    }
