module Group.View exposing (displayGroupTable, hGroupsColumn, tGroupsColumn)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Group.WithGroups exposing (WithGroups, hWithGroups, tWithGroups)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Ident.Identifiable exposing (gWithIdentifiers)
import Item.Item exposing (Item)
import Shared
import Typed.Typed exposing (Typed)
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


hGroupsColumn : Shared.Model -> Column (WithGroups (Hierarchic a)) msg
hGroupsColumn s =
    { header = headerCell color.table.header.background2 "Groups"
    , width = fill
    , view =
        hWithGroups s.state.grouped
            >> .groups
            >> Dict.values
            >> List.map (gWithIdentifiers s.state.groups s.state.groupTypes s.state.identifierTypes s.state.identifiers)
            >> List.map (tWithDisplay s.state.groups s.state.groupTypes s.state.configs SmallcardTitle)
            >> List.map .display
            >> List.map (Dict.get "SmallcardTitle" >> Maybe.withDefault "(missing zone config)")
            >> String.join "\n"
            >> text
            >> el [ height fill, padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ]
    }


tGroupsColumn : Shared.Model -> Column (WithGroups (Typed a)) msg
tGroupsColumn s =
    { header = headerCell color.table.header.background2 "Groups"
    , width = fill
    , view =
        tWithGroups s.state.grouped
            >> .groups
            >> Dict.values
            >> List.map (gWithIdentifiers s.state.groups s.state.groupTypes s.state.identifierTypes s.state.identifiers)
            >> List.map (tWithDisplay s.state.groups s.state.groupTypes s.state.configs SmallcardTitle)
            >> List.map .display
            >> List.map (Dict.get "SmallcardTitle" >> Maybe.withDefault "(missing zone config)")
            >> String.join "\n"
            >> text
            >> el [ height fill, padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ]
    }
