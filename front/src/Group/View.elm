module Group.View exposing (displayGroup)

import Element exposing (..)
import Entity.Type as EntityType exposing (Type)
import Group.Group as Group exposing (Group)
import Ident.View
import Prng.Uuid as Uuid exposing (Uuid)
import Shared
import View.Lang as Lang exposing (Lang(..))
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
