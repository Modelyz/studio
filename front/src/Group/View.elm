module Group.View exposing (displayGroupTable, groupsColumn)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Group.Group exposing (getGroups)
import Prng.Uuid exposing (Uuid)
import Shared
import Type exposing (Type)
import Typed.Type as TType
import View exposing (headerCell, innerCell)
import View.Style exposing (..)
import Zone.View exposing (display)
import Zone.Zone exposing (Zone(..))


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
            Debug.log "getGroups" (getGroups s.state.grouped uuid)
                |> List.map (display s.state.types s.state.configs SmallcardTitle s.state.identifiers (Type.TType TType.Group))
                |> String.join "\n"
                |> text
                |> el [ height fill, padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ]
    }
