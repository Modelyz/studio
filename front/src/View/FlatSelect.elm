module View.FlatSelect exposing (Config, flatSelect)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Prng.Uuid exposing (Uuid)
import Shared
import Type exposing (Type)
import View exposing (..)
import View.Smallcard exposing (tClickableCard, viewHalfCard)
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
        [ wrappedRow [ width <| minimum 50 shrink, height (px 48), Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
            [ h2 c.title
            , c.muuid
                |> Maybe.map (\uuid -> viewHalfCard (Just <| c.onInput Nothing) s.state.types s.state.configs s.state.identifiers c.what uuid)
                |> Maybe.withDefault (el [ padding 5, Font.color color.text.disabled ] (text "Empty"))
            ]
        , c.muuid |> Maybe.map (\_ -> none) |> Maybe.withDefault (h2 c.explain)
        , c.muuid
            |> Maybe.map (\_ -> none)
            |> Maybe.withDefault
                (wrappedRow [ padding 10, spacing 10 ]
                    (uuids
                        |> Dict.values
                        |> List.map (\uuid -> tClickableCard (c.onInput (Just uuid)) s.state.types s.state.configs s.state.identifiers c.what uuid)
                        |> withDefaultContent (p c.empty)
                    )
                )
        ]
