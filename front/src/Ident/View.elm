module Ident.View exposing (displayIdentifierDict, identifierColumn)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (IdentifierType)
import Prng.Uuid exposing (Uuid)
import Shared
import Type exposing (Type)
import View exposing (headerCell, innerCell)
import View.Style exposing (..)


displayIdentifierDict : String -> Dict String Identifier -> Element msg
displayIdentifierDict default data =
    -- TODO move closer from where it's used?
    if Dict.size data > 0 then
        table [ width shrink, Background.color color.item.background ]
            { data = Dict.values data
            , columns =
                [ { header = none, width = fill, view = .name >> innerCell }
                , { header = none, width = fill, view = Identifier.toValue >> innerCell }
                ]
            }

    else
        text default


identifierColumn : Shared.Model -> IdentifierType -> Column ( Uuid, Type, Maybe Uuid ) msg
identifierColumn s it =
    { header = headerCell color.table.header.background it.name
    , width = fill
    , view =
        \( uuid, _, _ ) ->
            s.state.identifiers
                |> Dict.values
                |> List.filter (\id -> id.name == it.name && id.identifiable == uuid)
                |> List.map Identifier.toValue
                |> List.head
                |> Maybe.withDefault ""
                |> innerCell
    }
