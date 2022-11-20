module Scope.View exposing (selectScope, toDisplay)

import Configuration as Config exposing (Configuration)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Group.Group exposing (Group)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (withIdentifiers)
import Ident.Identifier exposing (Identifier)
import Item.Item as Item exposing (Item)
import Prng.Uuid exposing (Uuid)
import Scope.Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Shared
import State exposing (State)
import Type exposing (Type)
import Typed.Type as TType
import Typed.Typed as T exposing (OnlyTyped, Typed)
import Value.Value exposing (Value)
import View exposing (..)
import View.Smallcard exposing (clickableCard, hItemClickableCard, sClickableCard, tItemClickableCard, viewHalfCard)
import View.Style exposing (..)
import Zone.View exposing (display)
import Zone.Zone exposing (Zone(..))


toDisplay : Dict String ( Type, Maybe Uuid ) -> Dict String Identifier -> Dict String Configuration -> Scope -> String
toDisplay types ids configs scope =
    -- TODO move to Scope.Scope?
    case scope of
        Empty ->
            "Nothing"

        IsItem (Type.TType tt) uuid ->
            (Type.toString (Type.TType tt) ++ ": ") ++ display types configs SmallcardTitle ids (Type.TType tt) uuid

        IsItem (Type.HType ht) uuid ->
            (Type.toString (Type.HType ht) ++ ": ") ++ display types configs SmallcardTitle ids (Type.HType ht) uuid

        HasType t ->
            "All " ++ Type.toPluralString t

        HasUserType t tuid ->
            "All " ++ Type.toPluralString t ++ " of type " ++ display types configs SmallcardTitle ids t tuid

        Identified _ ->
            "Identified"

        And s1 s2 ->
            "(" ++ toDisplay types ids configs s1 ++ ") And (" ++ toDisplay types ids configs s2 ++ ")"

        Or s1 s2 ->
            "(" ++ toDisplay types ids configs s1 ++ ") Or (" ++ toDisplay types ids configs s2 ++ ")"

        Not s ->
            "Not (" ++ toDisplay types ids configs s ++ ")"


selectScope : Shared.Model -> (Scope -> msg) -> Scope -> Element msg
selectScope s onInput scope =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            [ el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Apply to: "
            , (if scope == Empty then
                viewHalfCard Nothing

               else
                viewHalfCard (Just <| onInput Empty)
              )
                (text <| toDisplay s.state.types s.state.identifiers s.state.configs scope)
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
                    (h3 <| "of type:")
                        :: (s.state.types
                                |> Dict.filter (\_ ( t, muuid ) -> t == Type.TType tt)
                                |> Dict.values
                                |> List.map
                                    (\( t, muuid ) ->
                                        muuid
                                            |> Maybe.map (\uuid -> sClickableCard onInput s.state.types s.state.configs s.state.identifiers t uuid (HasUserType (Type.TType tt) uuid))
                                            |> Maybe.withDefault none
                                    )
                           )

                HasType (Type.HType ht) ->
                    (h3 <| "of type:")
                        :: (s.state.types
                                |> Dict.filter (\_ ( t, muuid ) -> t == Type.HType ht)
                                |> Dict.values
                                |> List.map
                                    (\( t, muuid ) ->
                                        muuid
                                            |> Maybe.map (\uuid -> sClickableCard onInput s.state.types s.state.configs s.state.identifiers t uuid (HasUserType (Type.HType ht) uuid))
                                            |> Maybe.withDefault none
                                    )
                           )

                _ ->
                    []
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            case scope of
                -- FIXME the 2 cases below now look the same (merge?)
                HasUserType (Type.HType ht) puuid ->
                    (h3 <| "You can select a specific one (otherwise click Next):")
                        :: (s.state.types
                                |> Dict.filter (\_ ( t, muuid ) -> muuid |> Maybe.map (\uuid -> uuid == puuid && containsScope s.state.types (IsItem t uuid) scope) |> Maybe.withDefault False)
                                |> Dict.values
                                |> List.map (\( _, muuid ) -> muuid |> Maybe.map (\uuid -> hItemClickableCard onInput s.state.types s.state.configs s.state.identifiers (Type.HType ht) uuid) |> Maybe.withDefault none)
                           )

                HasUserType (Type.TType tt) puuid ->
                    (h3 <| "You can select a specific one (otherwise click Next):")
                        :: (s.state.types
                                |> Dict.filter (\_ ( t, muuid ) -> muuid |> Maybe.map (\uuid -> uuid == puuid && containsScope s.state.types (IsItem t uuid) scope) |> Maybe.withDefault False)
                                |> Dict.values
                                |> List.map (\( _, muuid ) -> muuid |> Maybe.map (\uuid -> hItemClickableCard onInput s.state.types s.state.configs s.state.identifiers (Type.TType tt) uuid) |> Maybe.withDefault none)
                           )

                _ ->
                    []
        ]
