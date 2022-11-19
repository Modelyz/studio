module Flow.Input exposing (Config, input)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Flow exposing (Flow(..))
import Hierarchy.Hierarchic exposing (Hierarchic)
import Ident.Identifiable exposing (hWithIdentifiers, tWithIdentifiers)
import Ident.Identifier exposing (Identifier)
import Ident.IdentifierType exposing (IdentifierType)
import Resource.Resource exposing (Resource)
import Shared
import Typed.Typed exposing (OnlyTyped, Typed)
import Value.Input
import Value.Value exposing (Value)
import View exposing (..)
import View.Smallcard exposing (clickableCard, hClickableCard, hViewHalfCard, tClickableCard, tViewHalfCard)
import View.Style exposing (..)


type alias Config msg =
    { mflow : Maybe Flow
    , onInput : msg
    , onEnter : msg
    , title : String
    , explain : String
    , empty : String
    }


input : Config msg -> Shared.Model -> Element msg
input c s =
    let
        allTwithIdentifiers =
            s.state.resources
                |> Dict.map
                    (\_ t ->
                        { t
                            | identifiers =
                                s.state.identifiers
                                    |> Dict.filter (\_ id -> t.uuid == id.identifiable)
                        }
                    )

        allHwithIdentifiers =
            s.state.resourceTypes
                |> Dict.map
                    (\_ t ->
                        { t
                            | identifiers =
                                s.state.identifiers
                                    |> Dict.filter (\_ id -> t.uuid == id.identifiable)
                        }
                    )
    in
    c.mflow
        |> Maybe.map
            (\flow ->
                case flow of
                    ResourceFlow expr resource ->
                        column [ alignTop, spacing 10, width <| minimum 200 fill ]
                            [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                                [ h2 c.title

                                -- TODO
                                , text "edit flow expr"

                                {- Value.Input.inputExpression { onEnter = c.onEnter, onInput = c.onInput } s ( [], expr ) -}
                                , c.mflow
                                    |> Maybe.map (tWithIdentifiers allTwithIdentifiers allHwithIdentifiers s.state.identifierTypes s.state.identifiers)
                                    |> Maybe.map (tViewHalfCard (Just c.onInput) allTwithIdentifiers allHwithIdentifiers s.state.configs)
                                    |> Maybe.withDefault (el [ padding 5, Font.color color.text.disabled ] (text "Empty"))
                                ]
                            , h2 c.explain
                            , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ]
                                (allTwithIdentifiers
                                    |> Dict.values
                                    |> List.map (\t -> tClickableCard c.onInput allTwithIdentifiers allHwithIdentifiers s.state.configs t)
                                    |> withDefaultContent (p c.empty)
                                )
                            ]

                    ResourceTypeFlow expr resource ->
                        column [ alignTop, spacing 10, width <| minimum 200 fill ]
                            [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                                [ h2 c.title

                                -- TODO
                                , text "edit flow expr"

                                {- Value.Input.inputExpression { onEnter = c.onEnter, onInput = c.onInput } s ( [], expr ) -}
                                , c.mstuff
                                    |> Maybe.map (tWithIdentifiers allTwithIdentifiers allHwithIdentifiers s.state.identifierTypes s.state.identifiers)
                                    |> Maybe.map (tViewHalfCard (Just c.onInput) allTwithIdentifiers allHwithIdentifiers s.state.configs)
                                    |> Maybe.withDefault (el [ padding 5, Font.color color.text.disabled ] (text "Empty"))
                                ]
                            , h2 c.explain
                            , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ]
                                (allTwithIdentifiers
                                    |> Dict.values
                                    |> List.map (tClickableCard c.onInput allTwithIdentifiers allHwithIdentifiers s.state.configs)
                                    |> withDefaultContent (p c.empty)
                                )
                            ]
            )
