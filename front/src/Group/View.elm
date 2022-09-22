module Group.View exposing (displayGroupTable, groupsColumn)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Group.WithGroups exposing (WithGroups, withGroups)
import Ident.Identifiable exposing (withIdentifiers)
import Item.Item exposing (Item)
import Shared
import View exposing (headerCell, innerCell)
import View.Style exposing (..)
import Zone.View exposing (tWithDisplay)
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


groupsColumn : Shared.Model -> Column (WithGroups (Item i)) msg
groupsColumn s =
    { header = headerCell color.table.header.background2 "Groups"
    , width = fill
    , view =
        withGroups s.state.grouped
            >> .groups
            >> Dict.values
            >> List.map (withIdentifiers s.state.agents s.state.agentTypes s.state.identifierTypes s.state.identifiers)
            >> List.map (tWithDisplay s.state.groups s.state.groupTypes s.state.configs SmallcardTitle)
            >> List.map .display
            >> List.map (Dict.get "SmallcardTitle" >> Maybe.withDefault "(missing zone config)")
            >> String.join "\n"
            >> text
            >> el [ height fill, padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ]
    }
