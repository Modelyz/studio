module Type.View exposing (typeColumn)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Prng.Uuid exposing (Uuid)
import Shared
import Type exposing (Type, toType)
import View exposing (headerCell)
import View.Style exposing (..)
import Configuration.Zone.View exposing (displayZone)
import Configuration.Zone exposing (Zone(..))


typeColumn : Shared.Model -> Column ( Uuid, Type, Maybe Uuid ) msg
typeColumn s =
    { header = headerCell color.table.header.background3 "Type"
    , width = fill
    , view =
        \( _, t, mpuuid ) ->
            mpuuid
                |> Maybe.map (displayZone s.state SmallcardZone (toType t))
                |> Maybe.map (text >> el [ height fill, padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ])
                |> Maybe.withDefault (text "")
    }
