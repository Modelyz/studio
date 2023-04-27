module View.Table exposing (hView, tView)

import Dict
import Element exposing (..)
import Element.Background as Background
import Group.View exposing (groupsColumn)
import Hierarchy.Type as HType
import Ident.View exposing (identifierColumn)
import Prng.Uuid exposing (Uuid)
import Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Shared
import Type
import Type.View
import Typed.Type as TType
import View.Style exposing (..)


tView : Shared.Model -> Scope -> List { a | what : TType.Type, uuid : Uuid, type_ : Uuid } -> Element msg
tView s scope entities =
    wrappedRow
        [ spacing 10 ]
        [ table [ width fill, Background.color color.table.inner.background ]
            { data =
                entities
                    |> List.map (\e -> ( e.uuid, Type.TType e.what, Just e.type_ ))
            , columns =
                Type.View.typeColumn s
                    :: (s.state.identifierTypes
                            |> Dict.values
                            |> List.filter (\it -> containsScope s.state.types it.scope scope)
                            |> List.map (identifierColumn s)
                       )
                    ++ [ groupsColumn s ]
            }
        ]


hView : Shared.Model -> Scope -> List { a | what : HType.Type, uuid : Uuid, parent : Maybe Uuid } -> Element msg
hView s scope entityTypes =
    wrappedRow
        [ spacing 10 ]
        [ table [ width fill, Background.color color.table.inner.background ]
            { data =
                entityTypes
                    |> List.map (\h -> ( h.uuid, Type.HType h.what, h.parent ))
            , columns =
                Type.View.typeColumn s
                    :: (s.state.identifierTypes
                            |> Dict.values
                            |> List.filter (\it -> containsScope s.state.types it.scope scope)
                            |> List.map (identifierColumn s)
                       )
                    ++ [ groupsColumn s ]
            }
        ]
