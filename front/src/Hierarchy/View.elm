module Hierarchy.View exposing (toDesc)

import DictSet as Set exposing (DictSet)
import Element exposing (..)
import Element.Font as Font
import Hierarchy.Hierarchic as Hierarchic exposing (Hierarchic)
import Item.Item as Item exposing (Item)
import Prng.Uuid as Uuid exposing (Uuid)
import View.Style exposing (..)


toDesc : DictSet String (Hierarchic (Item a)) -> Hierarchic (Item a) -> Element msg
toDesc allHierarchic ht =
    -- used in AddPages to display the parent of the selected entity Type
    ht.parent
        |> Maybe.andThen (Item.find allHierarchic)
        |> Maybe.map
            (\rtp ->
                row [ Font.size size.text.small ]
                    [ text "Type: "
                    , text (Uuid.toString rtp.uuid)
                    ]
            )
        |> Maybe.map (el [ Font.size size.text.small ])
        |> Maybe.withDefault none
