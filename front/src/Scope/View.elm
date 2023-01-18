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
import Scope as Scope exposing (Scope(..))
import Shared
import Type exposing (Type)
import Typed.Type as TType
import View exposing (..)
import View.Smallcard exposing (clickableCard, hItemClickableCard, tClickableCard)
import View.Style exposing (..)
import Zone.View exposing (displayZone)
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

        Anything ->
            "Anything"

        IsItem (Type.TType tt) uuid ->
            (Type.toString (Type.TType tt) ++ ": ") ++ displayZone s.state types configs SmallcardTitle (Identifier.fromUuid uuid ids) s.state.grouped s.state.groups (Type.TType tt) uuid

        IsItem (Type.HType ht) uuid ->
            (Type.toString (Type.HType ht) ++ ": ") ++ displayZone s.state types configs SmallcardTitle (Identifier.fromUuid uuid ids) s.state.grouped s.state.groups (Type.HType ht) uuid

        HasType t ->
            Type.toPluralString t

        HasUserType (Type.TType tt) tuid ->
            TType.toPluralString tt ++ " of type: " ++ displayZone s.state types configs SmallcardTitle (Identifier.fromUuid tuid ids) s.state.grouped s.state.groups (Type.HType (TType.toHierarchic tt)) tuid

        HasUserType (Type.HType ht) tuid ->
            HType.toPluralString ht ++ " of type: " ++ displayZone s.state types configs SmallcardTitle (Identifier.fromUuid tuid ids) s.state.grouped s.state.groups (Type.HType ht) tuid

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


subScopes : Shared.Model -> Scope -> List Scope
subScopes s scope =
    case scope of
        Empty ->
            [ Anything ]

        Anything ->
            (HType.all |> List.map (\ht -> HasType (Type.HType ht)))
                ++ (TType.all |> List.map (\tt -> HasType (Type.TType tt)))

        HasType (Type.HType ht) ->
            s.state.types
                |> Dict.filter (\_ ( _, t, _ ) -> t == Type.HType ht)
                |> Dict.values
                |> List.map (\( uuid, _, _ ) -> HasUserType (Type.HType ht) uuid)

        HasType (Type.TType tt) ->
            s.state.types
                |> Dict.filter (\_ ( _, t, _ ) -> t == Type.HType (TType.toHierarchic tt))
                |> Dict.values
                |> List.map (\( uuid, _, _ ) -> HasUserType (Type.TType tt) uuid)

        HasUserType type_ puuid ->
            s.state.types
                |> Dict.filter (\_ ( uuid, t, _ ) -> t == type_ && H.isAscendantOf uuid s.state.types puuid)
                |> Dict.values
                |> List.map (\( uuid, _, _ ) -> IsItem type_ uuid)

        Or scope1 scope2 ->
            subScopes s scope1 ++ subScopes s scope2

        And scope1 scope2 ->
            Dict.values <|
                Dict.intersect
                    (Dict.fromList <| List.map (\sc -> ( Scope.toString sc, sc )) (subScopes s scope2))
                    (Dict.fromList <| List.map (\sc -> ( Scope.toString sc, sc )) (subScopes s scope1))

        _ ->
            []


selectScope : Shared.Model -> (Scope -> msg) -> Scope -> Scope -> String -> Element msg
selectScope s onInput sc startingScope title =
    let
        scope =
            if sc == Empty then
                startingScope

            else
                sc
    in
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            [ el [ paddingXY 10 0, Font.size size.text.h2 ] <| text title
            , halfCard (onInput Empty) s scope
            ]
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] (subScopes s scope |> List.map (\scc -> clickableCard (onInput scc) (text <| toDisplay s scc) none))
        ]
