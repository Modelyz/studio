module Scope.View exposing (selectScope, toDisplay)

import Configuration exposing (Configuration)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Hierarchy.Hierarchic as H
import Hierarchy.Type as HType
import Ident.Identifier as Identifier exposing (Identifier)
import Prng.Uuid exposing (Uuid)
import Scope.Scope exposing (Scope(..))
import Shared
import Type exposing (Type)
import Typed.Type as TType
import View exposing (..)
import View.Smallcard exposing (clickableCard, hItemClickableCard, tClickableCard)
import View.Style exposing (..)
import Zone.View exposing (display)
import Zone.Zone exposing (Zone(..))


toDisplay : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String Identifier -> Dict String Configuration -> Scope -> String
toDisplay types ids configs scope =
    -- TODO move to Scope.Scope?
    case scope of
        Empty ->
            "Nothing"

        IsItem (Type.TType tt) uuid ->
            (Type.toString (Type.TType tt) ++ ": ") ++ display types configs SmallcardTitle (Identifier.fromUuid uuid ids) (Type.TType tt) uuid

        IsItem (Type.HType ht) uuid ->
            (Type.toString (Type.HType ht) ++ ": ") ++ display types configs SmallcardTitle (Identifier.fromUuid uuid ids) (Type.HType ht) uuid

        HasType t ->
            "All " ++ Type.toPluralString t

        HasUserType (Type.TType tt) tuid ->
            "All " ++ TType.toPluralString tt ++ " of type " ++ display types configs SmallcardTitle (Identifier.fromUuid tuid ids) (Type.HType (TType.toHierarchic tt)) tuid

        HasUserType (Type.HType ht) tuid ->
            "All " ++ HType.toPluralString ht ++ " of type " ++ display types configs SmallcardTitle (Identifier.fromUuid tuid ids) (Type.HType ht) tuid

        Identified _ ->
            "Identified"

        And s1 s2 ->
            "(" ++ toDisplay types ids configs s1 ++ ") And (" ++ toDisplay types ids configs s2 ++ ")"

        Or s1 s2 ->
            "(" ++ toDisplay types ids configs s1 ++ ") Or (" ++ toDisplay types ids configs s2 ++ ")"

        Not s ->
            "Not (" ++ toDisplay types ids configs s ++ ")"


halfCard : msg -> Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String Configuration -> Dict String Identifier -> Scope -> Element msg
halfCard onDelete types configs ids scope =
    row [ Background.color color.item.selected ]
        [ el [ padding 10 ] (text <| toDisplay types ids configs scope)
        , if scope == Empty then
            none

          else
            button.secondary onDelete "×"
        ]


selectScope : Shared.Model -> (Scope -> msg) -> Scope -> Element msg
selectScope s onInput scope =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            [ el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Apply to: "
            , halfCard (onInput Empty) s.state.types s.state.configs s.state.identifiers scope
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
                        :: ((s.state.types
                                |> Dict.filter (\_ ( _, t, _ ) -> t == Type.HType (TType.toHierarchic tt))
                            )
                                |> Dict.values
                                |> List.map
                                    (\( uuid, _, _ ) ->
                                        let
                                            htype =
                                                Type.HType (TType.toHierarchic tt)
                                        in
                                        tClickableCard (onInput (HasUserType htype uuid)) s.state.types s.state.configs s.state.identifiers htype uuid
                                    )
                           )

                HasType (Type.HType ht) ->
                    (h3 <| "of type:")
                        :: (s.state.types
                                |> Dict.filter (\_ ( _, t, _ ) -> t == Type.HType ht)
                                |> Dict.values
                                |> List.map
                                    (\( uuid, _, _ ) ->
                                        let
                                            htype =
                                                Type.HType ht
                                        in
                                        tClickableCard (onInput (HasUserType htype uuid)) s.state.types s.state.configs s.state.identifiers htype uuid
                                    )
                           )

                _ ->
                    []

        -- then the possible choice of a specific entity as a IsItem
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            case scope of
                HasUserType (Type.HType ht) puuid ->
                    (h3 <| "You can select a specific one (otherwise click Next):")
                        :: (s.state.types
                                |> Dict.filter (\_ ( uuid, t, _ ) -> t == Type.HType ht && H.isAscendantOf uuid s.state.types puuid)
                                |> Dict.values
                                |> List.map (\( uuid, _, _ ) -> hItemClickableCard onInput s.state.types s.state.configs s.state.identifiers (Type.HType ht) uuid)
                           )

                HasUserType (Type.TType tt) puuid ->
                    (h3 <| "You can select a specific one (otherwise click Next):")
                        :: (s.state.types
                                |> Dict.filter (\_ ( uuid, t, _ ) -> t == Type.TType tt && H.isAscendantOf uuid s.state.types puuid)
                                |> Dict.values
                                |> List.map (\( uuid, _, _ ) -> hItemClickableCard onInput s.state.types s.state.configs s.state.identifiers (Type.TType tt) uuid)
                           )

                _ ->
                    []
        ]
