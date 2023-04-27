module View.Table exposing (hView, headerCell, innerCell, tView)

import Configuration.Zone exposing (Zone(..))
import Configuration.Zone.View exposing (displayZone)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Expression.Eval as Eval
import Expression.Rational as Rational
import Group.Group exposing (groupsOf)
import Hierarchy.Type as HType
import Ident.Identifier as Identifier
import Ident.IdentifierType exposing (IdentifierType)
import Prng.Uuid exposing (Uuid)
import Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import State exposing (State)
import Type exposing (Type, toType)
import Typed.Type as TType
import Value.ValueType exposing (ValueType)
import View.Style exposing (..)


headerCell : Color -> String -> Element msg
headerCell c =
    text >> el [ padding 5, Border.width 2, Border.color color.content.background, Background.color c ]


innerCell : String -> Element msg
innerCell =
    text >> el [ height fill, padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ]


tView : State -> Scope -> List { a | what : TType.Type, uuid : Uuid, type_ : Uuid } -> Element msg
tView s scope entities =
    wrappedRow
        [ spacing 10 ]
        [ table [ width fill, Background.color color.table.inner.background ]
            { data =
                entities
                    |> List.map (\e -> ( e.uuid, Type.TType e.what, Just e.type_ ))
            , columns =
                zoneColumn s
                    :: typeColumn s
                    :: (s.identifierTypes
                            |> Dict.values
                            |> List.filter (\it -> containsScope s.types it.scope scope)
                            |> List.map (identifierColumn s)
                       )
                    ++ (s.valueTypes
                            |> Dict.values
                            |> List.filter (\vt -> containsScope s.types vt.scope scope)
                            |> List.map (valueColumn s)
                       )
                    ++ [ groupsColumn s ]
            }
        ]


hView : State -> Scope -> List { a | what : HType.Type, uuid : Uuid, parent : Maybe Uuid } -> Element msg
hView s scope entityTypes =
    wrappedRow
        [ spacing 10 ]
        [ table [ width fill, Background.color color.table.inner.background ]
            { data =
                entityTypes
                    |> List.map (\h -> ( h.uuid, Type.HType h.what, h.parent ))
            , columns =
                zoneColumn s
                    :: typeColumn s
                    :: (s.identifierTypes
                            |> Dict.values
                            |> List.filter (\it -> containsScope s.types it.scope scope)
                            |> List.map (identifierColumn s)
                       )
                    ++ (s.valueTypes
                            |> Dict.values
                            |> List.filter (\vt -> containsScope s.types vt.scope scope)
                            |> List.map (valueColumn s)
                       )
                    ++ [ groupsColumn s ]
            }
        ]


identifierColumn : State -> IdentifierType -> Column ( Uuid, Type, Maybe Uuid ) msg
identifierColumn s it =
    { header = headerCell color.table.header.background it.name
    , width = fill
    , view =
        \( uuid, t, _ ) ->
            s.identifiers
                |> Dict.values
                |> List.filter (\id -> id.name == it.name && id.for == uuid && id.what == t && containsScope s.types (IsItem t uuid) it.scope)
                |> List.head
                |> Maybe.map Identifier.toValue
                |> Maybe.withDefault ""
                |> innerCell
    }


typeColumn : State -> Column ( Uuid, Type, Maybe Uuid ) msg
typeColumn s =
    { header = headerCell color.table.header.background3 "Type"
    , width = fill
    , view =
        \( _, t, mpuuid ) ->
            mpuuid
                |> Maybe.map (displayZone s SmallcardZone (toType t))
                |> Maybe.map (text >> el [ height fill, padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ])
                |> Maybe.withDefault (text "")
    }


groupsColumn : State -> Column ( Uuid, Type, Maybe Uuid ) msg
groupsColumn s =
    { header = headerCell color.table.header.background2 "Groups"
    , width = fill
    , view =
        \( uuid, _, _ ) ->
            groupsOf s.grouped uuid
                |> List.map (displayZone s SmallcardZone (Type.TType TType.Group))
                |> String.join "\n"
                |> text
                |> el [ height fill, padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ]
    }


valueColumn : State -> ValueType -> Column ( Uuid, Type, Maybe Uuid ) msg
valueColumn s vt =
    { header = headerCell color.table.header.background vt.name
    , width = fill
    , view =
        \( uuid, t, _ ) ->
            s.values
                |> Dict.values
                |> List.filter (\v -> vt.name == v.name && v.for == uuid && v.what == t && containsScope s.types (IsItem t uuid) vt.scope)
                |> List.head
                |> Maybe.andThen
                    (Eval.veval s { context = ( t, uuid ) } s.values
                        >> Result.map Rational.toFloatString
                        >> Result.toMaybe
                    )
                |> Maybe.withDefault ""
                |> innerCell
    }


zoneColumn : State -> Column ( Uuid, Type, Maybe Uuid ) msg
zoneColumn s =
    { header = headerCell color.table.header.background3 "Display Name"
    , width = fill
    , view =
        \( uuid, t, _ ) ->
            uuid
                |> displayZone s SmallcardZone t
                |> innerCell
    }
