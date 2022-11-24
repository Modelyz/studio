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


toDisplay : Shared.Model -> Scope -> String
toDisplay s scope =
    -- TODO move to Scope.Scope?
    let
        types =
            s.state.types

        configs =
            s.state.configs

        ids =
            s.state.identifiers
    in
    case scope of
        Empty ->
            "Nothing"

        IsItem (Type.TType tt) uuid ->
            (Type.toString (Type.TType tt) ++ ": ") ++ display types configs SmallcardTitle (Identifier.fromUuid uuid ids) (Type.TType tt) uuid

        IsItem (Type.HType ht) uuid ->
            (Type.toString (Type.HType ht) ++ ": ") ++ display types configs SmallcardTitle (Identifier.fromUuid uuid ids) (Type.HType ht) uuid

        HasType t ->
            Type.toPluralString t

        HasUserType (Type.TType tt) tuid ->
            TType.toPluralString tt ++ " of type: " ++ display types configs SmallcardTitle (Identifier.fromUuid tuid ids) (Type.HType (TType.toHierarchic tt)) tuid

        HasUserType (Type.HType ht) tuid ->
            HType.toPluralString ht ++ " of type: " ++ display types configs SmallcardTitle (Identifier.fromUuid tuid ids) (Type.HType ht) tuid

        Identified _ ->
            "Identified"

        And s1 s2 ->
            "(" ++ toDisplay s s1 ++ ") And (" ++ toDisplay s s2 ++ ")"

        Or s1 s2 ->
            "(" ++ toDisplay s s1 ++ ") Or (" ++ toDisplay s s2 ++ ")"

        Not sc ->
            "Not (" ++ toDisplay s sc ++ ")"


halfCard : msg -> Shared.Model -> Scope -> Element msg
halfCard onDelete s scope =
    row [ Background.color color.item.selected ]
        [ el [ padding 10 ] (text <| toDisplay s scope)
        , if scope == Empty then
            none

          else
            button.secondary onDelete "×"
        ]


selectScope : Shared.Model -> (Scope -> msg) -> Scope -> Scope -> String -> Element msg
selectScope s onInput sc forceScope title =
    let
        scope =
            if sc == Empty then
                forceScope

            else
                sc
    in
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            [ el [ paddingXY 10 0, Font.size size.text.h2 ] <| text title
            , halfCard (onInput Empty) s scope
            ]
        , if scope == Empty then
            column [ spacing 10 ]
                [ -- First the concrete types
                  wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
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
                    (h3 <| "Choose among:")
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
                                        tClickableCard (onInput (HasUserType (Type.TType tt) uuid)) s.state.types s.state.configs s.state.identifiers htype uuid
                                    )
                                |> withDefaultContent (text <| "(There are no " ++ TType.toString tt ++ " Types defined)")
                           )

                HasType (Type.HType ht) ->
                    (h3 <| "Choose among:")
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
                                |> withDefaultContent (text <| "(There are no " ++ HType.toString ht ++ " defined)")
                           )

                Or (HasType (Type.TType tt)) (HasType (Type.HType ht)) ->
                    [ column [ alignTop, spacing 20, width <| minimum 200 fill ]
                        [ h3 <| "You can restrict to a more specific " ++ TType.toString tt ++ " of Type:"
                        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
                            (s.state.types
                                |> Dict.filter (\_ ( _, t, _ ) -> t == Type.HType (TType.toHierarchic tt))
                                |> Dict.values
                                |> List.map
                                    (\( uuid, _, _ ) ->
                                        let
                                            htype =
                                                Type.HType (TType.toHierarchic tt)
                                        in
                                        tClickableCard (onInput (HasUserType (Type.TType tt) uuid)) s.state.types s.state.configs s.state.identifiers htype uuid
                                    )
                            )
                        , h3 <| "or restrict to a " ++ HType.toString ht ++ " of Type:"
                        , wrappedRow
                            [ padding 10, spacing 10, Border.color color.item.border ]
                            (s.state.types
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
                        ]
                    ]

                _ ->
                    []

        -- then the possible choice of a specific entity as a IsItem
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            case scope of
                HasUserType (Type.HType ht) puuid ->
                    (h3 <| "You may select a specific one (otherwise click Next):")
                        :: (s.state.types
                                |> Dict.filter (\_ ( uuid, t, _ ) -> t == Type.HType ht && H.isAscendantOf uuid s.state.types puuid)
                                |> Dict.values
                                |> List.map (\( uuid, _, _ ) -> hItemClickableCard onInput s.state.types s.state.configs s.state.identifiers (Type.HType ht) uuid)
                           )

                HasUserType (Type.TType tt) puuid ->
                    (h3 <| "You may select a specific one (otherwise click Next):")
                        :: (s.state.types
                                |> Dict.filter (\_ ( uuid, t, _ ) -> t == Type.TType tt && H.isAscendantOf uuid s.state.types puuid)
                                |> Dict.values
                                |> List.map (\( uuid, _, _ ) -> hItemClickableCard onInput s.state.types s.state.configs s.state.identifiers (Type.TType tt) uuid)
                           )

                _ ->
                    []
        ]
