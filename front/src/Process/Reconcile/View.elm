module Process.Reconcile.View exposing (view)

import Configuration.Zone exposing (Zone(..))
import Configuration.Zone.View exposing (displayZone)
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


view : State -> (Uuid -> msg) -> (Reconciliation -> msg) -> Reconciliation -> Element msg
view s onChoose onDelete r =
    clickableRemovableCard
        (onChoose r.event)
        (onDelete r)
        (text <| Rational.toFloatString r.qty ++ " / " ++ displayZone s SmallcardZone (Type.TType TType.Event) r.event)
        (Dict.get (Uuid.toString r.event) s.types
            |> Maybe.andThen third
            |> Maybe.map (\puuid -> displayZone s SmallcardZone {- TODO check?(Identifier.fromUuid puuid ids) -} (Type.HType HType.EventType) puuid)
            |> Maybe.withDefault ""
            |> text
        )