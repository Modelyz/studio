module Group.View exposing (displayGroup, displayGroupTable)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Entity.Type as EntityType exposing (Type)
import Group.Group as Group exposing (Group)
import Ident.View
import Prng.Uuid as Uuid exposing (Uuid)
import Shared
import View exposing (headerCell, innerCell)
import View.Lang as Lang exposing (Lang(..))
import View.Style exposing (..)
import Zone.Zone as Zone exposing (Zone(..))


displayGroup : Shared.Model -> Group -> Element msg
displayGroup s g =
    -- TODO refactor to avoid Element here
    row []
        [ text <|
            -- TODO display identifiers of the identifiable group
            Uuid.toString g.uuid
                ++ " of type "

        -- TODO also display the group type
        --, Group.fromUuid s.state.groups g.type_
        --    |> Maybe.map (Ident.View.display s SmallcardTitle FR_fr)
        --    |> Maybe.withDefault (text "(deleted type)")
        ]


displayGroupTable : String -> List String -> Element msg
displayGroupTable default groups =
    if List.length groups > 0 then
        table [ width shrink, Background.color color.item.background ]
            { data = groups
            , columns =
                [ { header = headerCell "Groups", width = fill, view = innerCell }
                ]
            }

    else
        text default
