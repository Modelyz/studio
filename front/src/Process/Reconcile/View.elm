module Process.Reconcile.View exposing (view)

import Dict
import Element exposing (..)
import Expression.Rational as Rational
import Hierarchy.Type as HType
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Reconcile exposing (Reconciliation)
import State exposing (State)
import Type
import Typed.Type as TType
import Util exposing (third)
import View.Smallcard exposing (clickableRemovableCard)
import Zone.View
import Zone.Zone exposing (Zone(..))


view : State -> (Uuid -> msg) -> (Reconciliation -> msg) -> Reconciliation -> Element msg
view s onChoose onDelete r =
    let
        event =
            Dict.get (Uuid.toString r.event) s.events
    in
    clickableRemovableCard
        (onChoose r.event)
        (onDelete r)
        (text <| Rational.toFloatString r.qty ++ " / " ++ Zone.View.displayZone s SmallcardTitle (Type.TType TType.Event) r.event)
        (Dict.get (Uuid.toString r.event) s.types
            |> Maybe.andThen third
            |> Maybe.map (\puuid -> Zone.View.displayZone s SmallcardTitle {- FIXME?(Identifier.fromUuid puuid ids) -} (Type.HType HType.EventType) puuid)
            |> Maybe.withDefault ""
            |> text
        )
