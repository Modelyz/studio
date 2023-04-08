module Scope.View exposing (selectScope, toDisplay)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Hierarchy.Hierarchic as H
import Hierarchy.Type as HType
import Scope as Scope exposing (Scope(..))
import State exposing (State)
import Type
import Typed.Type as TType
import View exposing (..)
import View.Smallcard exposing (clickableCard)
import View.Style exposing (..)
import Configuration.Zone.View exposing (displayZone)
import Configuration.Zone exposing (Zone(..))


toDisplay : State -> Scope -> String
toDisplay s scope =
    -- TODO move to Scope.Scope?
    case scope of
        Empty ->
            "Nothing"

        Anything ->
            "Anything"

        IsItem (Type.TType tt) uuid ->
            (Type.toString (Type.TType tt) ++ ": ") ++ displayZone s SmallcardZone {- TODO check?(Identifier.fromUuid uuid ids) -} (Type.TType tt) uuid

        IsItem (Type.HType ht) uuid ->
            (Type.toString (Type.HType ht) ++ ": ") ++ displayZone s SmallcardZone {- TODO check?(Identifier.fromUuid uuid ids) -} (Type.HType ht) uuid

        HasType t ->
            Type.toPluralString t

        HasUserType (Type.TType tt) tuid ->
            TType.toPluralString tt ++ " of type: " ++ displayZone s SmallcardZone {- (TODO check?Identifier.fromUuid tuid ids) -} (Type.HType (TType.toHierarchic tt)) tuid

        HasUserType (Type.HType ht) tuid ->
            HType.toPluralString ht ++ " of type: " ++ displayZone s SmallcardZone {- TODO check?(Identifier.fromUuid tuid ids) -} (Type.HType ht) tuid

        Identified _ ->
            "Identified"

        And s1 s2 ->
            "(" ++ toDisplay s s1 ++ ") And (" ++ toDisplay s s2 ++ ")"

        Or s1 s2 ->
            "(" ++ toDisplay s s1 ++ ") Or (" ++ toDisplay s s2 ++ ")"

        Not sc ->
            "Not (" ++ toDisplay s sc ++ ")"


halfCard : msg -> State -> Scope -> Element msg
halfCard onDelete s scope =
    row [ Background.color color.item.selected ]
        [ el [ padding 10 ] (text <| toDisplay s scope)
        , if scope == Empty then
            none

          else
            button.secondary (Ok onDelete) "×"
        ]


subScopes : State -> Scope -> List Scope
subScopes s scope =
    case scope of
        Empty ->
            [ Anything ]

        Anything ->
            (HType.all |> List.map (\ht -> HasType (Type.HType ht)))
                ++ (TType.all |> List.map (\tt -> HasType (Type.TType tt)))

        HasType (Type.HType ht) ->
            s.types
                |> Dict.filter (\_ ( _, t, _ ) -> t == Type.HType ht)
                |> Dict.values
                |> List.map (\( uuid, _, _ ) -> HasUserType (Type.HType ht) uuid)

        HasType (Type.TType tt) ->
            s.types
                |> Dict.filter (\_ ( _, t, _ ) -> t == Type.HType (TType.toHierarchic tt))
                |> Dict.values
                |> List.map (\( uuid, _, _ ) -> HasUserType (Type.TType tt) uuid)

        HasUserType type_ puuid ->
            s.types
                |> Dict.filter (\_ ( uuid, t, _ ) -> t == type_ && H.isAscendantOf uuid s.types puuid)
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


selectScope : State -> (Scope -> msg) -> Scope -> Scope -> String -> Element msg
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
