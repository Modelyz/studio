module Scope.View exposing (selectScope, toDisplay)

import Configuration as Config exposing (Configuration)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Group.Group exposing (Group)
import Group.WithGroups exposing (WithGroups)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (withIdentifiers)
import Ident.Identifier exposing (Identifier)
import Item.Item as Item exposing (Item)
import Prng.Uuid exposing (Uuid)
import Scope.Scope exposing (Scope(..))
import Shared
import State exposing (State)
import Type
import Typed.Type as TType
import Typed.Typed as T exposing (OnlyTyped, Typed)
import Value.Value exposing (Value)
import View exposing (..)
import View.Smallcard exposing (clickableCard, hItemClickableCard, sClickableCard, tItemClickableCard, viewHalfCard)
import View.Style exposing (..)
import Zone.View exposing (hDisplay, tDisplay)
import Zone.Zone exposing (Zone(..))


toDisplay : Dict String OnlyTyped -> Dict String (Hierarchic b) -> Dict String Configuration -> Scope -> String
toDisplay allT allH configs scope =
    -- for user display
    -- TODO resolve the uuids
    case scope of
        Empty ->
            "Nothing"

        IsItem (Type.TType tt) uuid ->
            T.find allT uuid
                |> Maybe.map (\t -> (Type.toString (Type.TType tt) ++ ": ") ++ tDisplay allT allH configs SmallcardTitle t)
                |> Maybe.withDefault "(missing)"

        IsItem (Type.HType ht) uuid ->
            H.find allH uuid
                |> Maybe.map (\h -> (Type.toString (Type.HType ht) ++ ": ") ++ hDisplay allT allH configs SmallcardTitle h)
                |> Maybe.withDefault "(missing)"

        HasType t ->
            "All " ++ Type.toPluralString t

        HasUserType ht tuid ->
            "All " ++ HType.toPluralString ht ++ " of type " ++ (H.find allH tuid |> Maybe.map (hDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "(missing)")

        Identified _ ->
            "Identified"

        And s1 s2 ->
            -- FIXME allT and allH should be different between the left and right part
            "(" ++ toDisplay allT allH configs s1 ++ ") And (" ++ toDisplay allT allH configs s2 ++ ")"

        Or s1 s2 ->
            "(" ++ toDisplay allT allH configs s1 ++ ") Or (" ++ toDisplay allT allH configs s2 ++ ")"

        Not s ->
            "Not (" ++ toDisplay allT allH configs s ++ ")"


selectScope : Shared.Model -> (Scope -> msg) -> Scope -> Element msg
selectScope s onInput scope =
    -- TODO replace s with allT allH?
    -- TODO refactor
    let
        allT =
            case scope of
                HasType (Type.TType tt) ->
                    State.allTyped s.state tt |> withIdentifiers s.state

                HasType (Type.HType ht) ->
                    State.allTyped s.state (TType.fromHierarchic ht) |> withIdentifiers s.state

                HasUserType ht _ ->
                    State.allTyped s.state (TType.fromHierarchic ht) |> withIdentifiers s.state

                IsItem (Type.TType tt) uuid ->
                    State.allTyped s.state tt |> withIdentifiers s.state

                _ ->
                    Dict.empty

        allH =
            case scope of
                HasType (Type.TType tt) ->
                    State.allHierarchic s.state (TType.toHierarchic tt) |> withIdentifiers s.state

                HasType (Type.HType ht) ->
                    State.allHierarchic s.state ht |> withIdentifiers s.state

                HasUserType ht _ ->
                    State.allHierarchic s.state ht |> withIdentifiers s.state

                IsItem (Type.HType ht) uuid ->
                    State.allHierarchic s.state ht |> withIdentifiers s.state

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
                            -- TODO try to replace with allT from above
                            State.allHierarchic s.state (TType.toHierarchic tt)
                                |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })
                    in
                    (h3 <| "of type:")
                        :: (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (\h -> sClickableCard onInput allT allHwithIdentifiers s.state.configs h (TType.toHierarchic tt))
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
                                |> List.map (\h -> sClickableCard onInput allT allHwithIdentifiers s.state.configs h ht)
                           )

                _ ->
                    []
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            case scope of
                HasUserType ht uuid ->
                    let
                        allHwithIdentifiers =
                            State.allHierarchic s.state ht |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })
                    in
                    (h3 <| "You can select a specific Hierarchic one (otherwise click Next):") :: (allHwithIdentifiers |> Dict.values |> List.map (\h -> hItemClickableCard onInput allT allHwithIdentifiers s.state.configs h (Type.HType ht)))

                _ ->
                    []
        ]
