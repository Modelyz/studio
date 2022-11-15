module View.FlatSelect exposing (TConfig, hFlatselect, tFlatselect)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Hierarchy.Hierarchic exposing (Hierarchic)
import Ident.Identifiable exposing (hWithIdentifiers, tWithIdentifiers)
import Ident.Identifier exposing (Identifier)
import Ident.IdentifierType exposing (IdentifierType)
import Shared
import Typed.Typed exposing (OnlyTyped, Typed)
import View exposing (..)
import View.Smallcard exposing (clickableCard, hClickableCard, hViewHalfCard, tClickableCard, tViewHalfCard)
import View.Style exposing (..)


type alias TConfig a b msg =
    { allT : Shared.Model -> Dict String (Typed a)
    , allH : Shared.Model -> Dict String (Hierarchic b)
    , mstuff : Maybe (Typed a)
    , onInput : Maybe (Typed a) -> msg
    , title : String
    , explain : String
    , empty : String
    }


type alias HConfig a b msg =
    { allT : Shared.Model -> Dict String (Typed a)
    , allH : Shared.Model -> Dict String (Hierarchic b)
    , mstuff : Maybe (Hierarchic b)
    , onInput : Maybe (Hierarchic b) -> msg
    , title : String
    , explain : String
    , empty : String
    }



-- TODO remove?
-- TODO : try to remove modules with a partial Model by passing the target (such as flatselect here)


tFlatselect : TConfig a b msg -> Shared.Model -> Element msg
tFlatselect c s =
    let
        allTwithIdentifiers =
            c.allT s
                |> Dict.map
                    (\_ t ->
                        { t
                            | identifiers =
                                s.state.identifiers
                                    |> Dict.filter (\_ id -> t.uuid == id.identifiable)
                        }
                    )
    in
    column [ alignTop, spacing 10, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
            [ h2 c.title
            , c.mstuff
                |> Maybe.map (tWithIdentifiers (c.allT s) (c.allH s) s.state.identifierTypes s.state.identifiers)
                |> Maybe.map (tViewHalfCard (c.onInput Nothing) allTwithIdentifiers (c.allH s) s.state.configs)
                |> Maybe.withDefault (el [ padding 5, Font.color color.text.disabled ] (text "Empty"))
            ]
        , h2 c.explain
        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ]
            (allTwithIdentifiers
                |> Dict.values
                |> List.map (tClickableCard c.onInput allTwithIdentifiers (c.allH s) s.state.configs)
                |> withDefaultContent (p c.empty)
            )
        ]


hFlatselect : HConfig a b msg -> Shared.Model -> Element msg
hFlatselect c s =
    let
        allHwithIdentifiers =
            c.allH s
                |> Dict.map
                    (\_ h ->
                        { h
                            | identifiers =
                                s.state.identifiers
                                    |> Dict.filter (\_ id -> h.uuid == id.identifiable)
                        }
                    )
    in
    column [ alignTop, spacing 10, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
            [ h2 c.title
            , c.mstuff
                |> Maybe.map (hWithIdentifiers (c.allT s) (c.allH s) s.state.identifierTypes s.state.identifiers)
                |> Maybe.map (hViewHalfCard (c.onInput Nothing) (c.allT s) allHwithIdentifiers s.state.configs)
                |> Maybe.withDefault (el [ padding 5, Font.color color.text.disabled ] (text "Empty"))
            ]
        , h2 c.explain
        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ]
            (allHwithIdentifiers
                |> Dict.values
                |> List.map (hClickableCard c.onInput (c.allT s) allHwithIdentifiers s.state.configs)
                |> withDefaultContent (p c.empty)
            )
        ]
