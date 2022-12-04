module Type.View exposing (typeColumn)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Prng.Uuid exposing (Uuid)
import Shared
import Type exposing (Type, toType, typeOf)
import Typed.Type as TType
import View exposing (headerCell, innerCell)
import View.Style exposing (..)
import Zone.View exposing (display)
import Zone.Zone exposing (Zone(..))


typeColumn : Shared.Model -> Column ( Uuid, Type, Maybe Uuid ) msg
typeColumn s =
    { header = headerCell color.table.header.background3 "Type"
    , width = fill
    , view =
        \( _, t, mpuuid ) ->
            mpuuid
                |> Maybe.map (display s.state.types s.state.configs SmallcardTitle s.state.identifiers s.state.grouped (toType t))
                |> Maybe.map (text >> el [ height fill, padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ])
                |> Maybe.withDefault (text "")
    }
