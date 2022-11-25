module Flow.Input exposing (Config, input)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Expression.Input
import Expression.Rational exposing (Rational)
import Flow exposing (Flow(..))
import Hierarchy.Type as HType
import Ident.Identifier exposing (Identifier)
import Ident.IdentifierType exposing (IdentifierType)
import Resource.Resource exposing (Resource)
import Scope.Scope as Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Shared
import Type
import Typed.Type as TType
import Value.Input
import Value.Value exposing (Value)
import View exposing (..)
import View.Smallcard exposing (clickableCard, tClickableCard, viewHalfCard)
import View.Style exposing (..)


type alias Config msg =
    { flow : Maybe Flow
    , scope : Scope
    , onSelect : Maybe Flow -> msg
    , onEnter : msg
    }


input : Config msg -> Shared.Model -> Element msg
input c s =
    column [ spacing 20 ]
        [ wrappedRow [ width <| minimum 50 shrink, height (px 48), Border.width 2, padding 3, spacing 4, Border.color color.item.border ]
            [ c.flow
                |> Maybe.map
                    (\flow ->
                        case flow of
                            ResourceFlow r ->
                                viewHalfCard (Just <| c.onSelect Nothing) s.state.types s.state.configs s.state.identifiers (Type.TType r.what) r.uuid

                            ResourceTypeFlow rt ->
                                viewHalfCard (Just <| c.onSelect Nothing) s.state.types s.state.configs s.state.identifiers (Type.HType rt.what) rt.uuid
                    )
                |> Maybe.withDefault (text "Nothing chosen yet")
            ]
        , c.flow
            |> Maybe.map (\_ -> none)
            |> Maybe.withDefault
                (column [ spacing 10 ]
                    [ column [ alignTop, padding 10, spacing 10, width <| minimum 200 fill, Background.color color.content.choice ]
                        [ h2 "Choose either a specific Resource:"
                        , wrappedRow [ padding 10, spacing 10 ]
                            (s.state.resources
                                |> Dict.filter (\_ r -> containsScope s.state.types (IsItem (Type.TType r.what) r.uuid) c.scope)
                                |> Dict.values
                                |> List.map (\r -> tClickableCard (c.onSelect (Just <| ResourceFlow r)) s.state.types s.state.configs s.state.identifiers (Type.TType r.what) r.uuid)
                                |> withDefaultContent (p "(No Resources to choose from)")
                            )
                        ]
                    , column [ alignTop, padding 10, spacing 10, width <| minimum 200 fill, Background.color color.content.choice ]
                        [ h2 "Or a general Resource Type:"
                        , wrappedRow [ padding 10, spacing 10 ]
                            (s.state.resourceTypes
                                |> Dict.filter (\_ rt -> containsScope s.state.types (IsItem (Type.HType rt.what) rt.uuid) c.scope)
                                |> Dict.values
                                |> List.map (\rt -> tClickableCard (c.onSelect (Just <| ResourceTypeFlow rt)) s.state.types s.state.configs s.state.identifiers (Type.HType rt.what) rt.uuid)
                                |> withDefaultContent (p "(No Resource Types to choose from)")
                            )
                        ]
                    ]
                )
        ]
