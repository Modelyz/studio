module View.FlatSelect exposing (Config, flatSelect)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Hierarchy.Hierarchic exposing (Hierarchic)
import Ident.Identifiable exposing (getIdentifiers)
import Ident.Identifier exposing (Identifier)
import Ident.IdentifierType exposing (IdentifierType)
import Prng.Uuid as Uuid exposing (Uuid)
import Shared
import Type exposing (Type)
import Typed.Typed exposing (OnlyTyped, Typed)
import View exposing (..)
import View.Smallcard exposing (clickableCard, hClickableCard, hViewHalfCard, tClickableCard, tViewHalfCard)
import View.Style exposing (..)


type alias Config msg =
    { what : Type
    , muuid : Maybe Uuid
    , onInput : Maybe Uuid -> msg
    , title : String
    , explain : String
    , empty : String
    }


flatSelect : Shared.Model -> Config msg -> Dict String Uuid -> Element msg
flatSelect s c uuids =
    column [ alignTop, spacing 10, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
            [ h2 c.title
            , c.muuid
                |> Maybe.map (\uuid -> tViewHalfCard (c.onInput Nothing) s.state.types s.state.configs s.state.identifiers c.what uuid)
                |> Maybe.withDefault (el [ padding 5, Font.color color.text.disabled ] (text "Empty"))
            ]
        , h2 c.explain
        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ]
            (uuids
                |> Dict.values
                |> List.map (\uuid -> tClickableCard (c.onInput (Just uuid)) s.state.types s.state.configs s.state.identifiers c.what uuid)
                |> withDefaultContent (p c.empty)
            )
        ]
