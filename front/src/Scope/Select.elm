module Scope.Select exposing (selectScope)

import Configuration as Config exposing (Configuration)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Hierarchy.Type as HType
import Item.Item as Item
import Scope.Scope exposing (Scope(..))
import Scope.View exposing (toDisplay)
import Shared
import State
import Type
import Typed.Type as TType
import Typed.Typed exposing (Typed)
import View exposing (..)
import View.Smallcard exposing (clickableCard, hItemClickableCard, sClickableCard, tItemClickableCard, viewHalfCard)
import View.Style exposing (..)
import Zone.View exposing (display)
import Zone.Zone exposing (Zone(..))


selectScope : Shared.Model -> (Scope -> msg) -> Scope -> Element msg
selectScope s onInput scope =
    -- TODO replace s with allT allH?
    -- TODO refactor
    let
        allT =
            case scope of
                HasType t ->
                    case t of
                        Type.TType tt ->
                            State.allTyped s.state tt
                                |> Dict.map (\_ x -> { x | identifiers = s.state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })

                        Type.HType ht ->
                            State.allTyped s.state (TType.fromHierarchic ht)
                                |> Dict.map (\_ x -> { x | identifiers = s.state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })

                HasUserType t _ ->
                    case t of
                        Type.TType tt ->
                            State.allTyped s.state tt
                                |> Dict.map (\_ x -> { x | identifiers = s.state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })

                        Type.HType ht ->
                            State.allTyped s.state (TType.fromHierarchic ht)
                                |> Dict.map (\_ x -> { x | identifiers = s.state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })

                IsItem (Type.TType tt) uuid ->
                    State.allTyped s.state tt
                        |> Dict.map (\_ x -> { x | identifiers = s.state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })

                _ ->
                    Dict.empty

        allH =
            case scope of
                HasType t ->
                    case t of
                        Type.TType tt ->
                            State.allHierarchic s.state (TType.toHierarchic tt)
                                |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })

                        Type.HType ht ->
                            State.allHierarchic s.state ht
                                |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })

                HasUserType t _ ->
                    case t of
                        Type.TType tt ->
                            State.allHierarchic s.state (TType.toHierarchic tt) |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })

                        Type.HType ht ->
                            State.allHierarchic s.state ht |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })

                IsItem (Type.HType ht) uuid ->
                    State.allHierarchic s.state ht
                        |> Dict.map (\_ x -> { x | identifiers = s.state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })

                _ ->
                    Dict.empty
    in
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            [ el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Apply to: "
            , (if scope == Empty then
                viewHalfCard Nothing

               else
                viewHalfCard (Just <| onInput Empty)
              )
                (text <| toDisplay allT allH s.state.configs scope)
            ]
        , if scope == Empty then
            column [ spacing 10 ]
                [ h2 <| "What should it apply to?"

                -- First the concrete types
                , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
                    (TType.all
                        |> List.map
                            (\t ->
                                clickableCard (onInput (HasType (Type.TType t))) (text <| TType.toPluralString t) none
                            )
                    )
                , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
                    (HType.all
                        |> List.map
                            (\t ->
                                clickableCard (onInput (HasType (Type.HType t))) (text <| HType.toPluralString t) none
                            )
                    )
                ]

          else
            none

        -- then the choice of user type, depending on the previously selected concrete type
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            case scope of
                HasType (Type.TType tt) ->
                    let
                        -- all the typed items
                        -- all the hierarchic items corresponding to the typed ones
                        allHwithIdentifiers =
                            State.allHierarchic s.state (TType.toHierarchic tt)
                                |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })
                    in
                    (h3 <| "of type:")
                        :: (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (\h -> sClickableCard onInput allT allHwithIdentifiers s.state.configs h (Type.TType tt))
                           )

                HasType (Type.HType ht) ->
                    -- TODO this should be always be displayed to select a subtype of the hierarchic
                    let
                        allHwithIdentifiers =
                            State.allHierarchic s.state ht
                                |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })
                    in
                    (h3 <| "of type:")
                        :: (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (\h -> sClickableCard onInput allT allHwithIdentifiers s.state.configs h (Type.HType ht))
                           )

                _ ->
                    []
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            case scope of
                HasUserType (Type.TType tt) uuid ->
                    let
                        allTwithIdentifiers =
                            State.allTyped s.state tt |> Dict.map (\_ t -> { t | identifiers = s.state.identifiers |> Dict.filter (\_ id -> t.uuid == id.identifiable) })
                    in
                    (h3 <| "You can select a specific Typed one (otherwise click Next):") :: (allTwithIdentifiers |> Dict.values |> List.map (\t -> tItemClickableCard onInput allTwithIdentifiers allH s.state.configs t (Type.TType tt)))

                HasUserType (Type.HType ht) uuid ->
                    let
                        allHwithIdentifiers =
                            State.allHierarchic s.state ht |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })
                    in
                    (h3 <| "You can select a specific Hierarchic one (otherwise click Next):") :: (allHwithIdentifiers |> Dict.values |> List.map (\h -> hItemClickableCard onInput allT allHwithIdentifiers s.state.configs h (Type.HType ht)))

                _ ->
                    []
        ]