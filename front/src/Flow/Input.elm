module Flow.Input exposing (Config, input)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Flow exposing (Flow(..))
import Ident.Identifier exposing (Identifier)
import Ident.IdentifierType exposing (IdentifierType)
import Resource.Resource exposing (Resource)
import Scope.Scope as Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Shared
import Type
import Value.Input
import Value.Rational exposing (Rational)
import Value.Value exposing (Value)
import View exposing (..)
import View.Smallcard exposing (clickableCard, tClickableCard)
import View.Style exposing (..)


type alias Config msg =
    { flow : Flow
    , scope : Scope
    , onInput : Flow -> msg
    , onSelect : Flow -> msg
    , onEnter : msg
    , title : String
    , explain : String
    , empty : String
    }


input : Config msg -> Shared.Model -> Element msg
input c s =
    case c.flow of
        ResourceFlow expr resource ->
            column [ alignTop, spacing 10, width <| minimum 200 fill ]
                [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                    [ h2 c.title

                    -- TODO
                    , text "edit r flow expr"

                    {- Value.Input.inputExpression { onEnter = c.onEnter, onInput = c.onInput } s ( [], expr ) -}
                    ]
                , h2 c.explain
                , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ]
                    (s.state.resources
                        |> Dict.filter (\_ r -> containsScope s.state.types (IsItem (Type.TType r.what) r.uuid) c.scope)
                        |> Dict.values
                        |> List.map (\r -> tClickableCard (c.onSelect (ResourceFlow expr r)) s.state.types s.state.configs s.state.identifiers (Type.TType r.what) r.uuid)
                        |> withDefaultContent (p c.empty)
                    )
                ]

        ResourceTypeFlow expr resourceType ->
            column [ alignTop, spacing 10, width <| minimum 200 fill ]
                [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                    [ h2 c.title

                    -- TODO
                    , text "edit rt flow expr"

                    {- Value.Input.inputExpression { onEnter = c.onEnter, onInput = c.onInput } s ( [], expr ) -}
                    ]
                , h2 c.explain
                , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ]
                    (s.state.resourceTypes
                        |> Dict.filter (\_ rt -> containsScope s.state.types (IsItem (Type.HType rt.what) rt.uuid) c.scope)
                        |> Dict.values
                        |> List.map (\rt -> tClickableCard (c.onSelect (ResourceTypeFlow expr rt)) s.state.types s.state.configs s.state.identifiers (Type.HType rt.what) rt.uuid)
                        |> withDefaultContent (p c.empty)
                    )
                ]

        None ->
            none
