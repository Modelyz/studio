module Scope.View exposing (toDisplay)

import Configuration as Config exposing (Configuration)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Hierarchy.Type as HType
import Item.Item as Item
import Scope.Scope exposing (Scope(..))
import Shared
import State
import Type
import Typed.Type as TType
import Typed.Typed exposing (Typed)
import View exposing (..)
import View.Smallcard exposing (clickableCard, hItemClickableCard, sClickableCard, tItemClickableCard, viewHalfCard)
import View.Style exposing (..)
import Zone.View exposing (display)
import Zone.Zone exposing (Zone(..))


toDisplay : Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Scope -> String
toDisplay allT allH configs scope =
    -- for user display
    -- TODO resolve the uuids
    case scope of
        Empty ->
            "Nothing"

        IsItem t uuid ->
            let
                mconfig =
                    Config.getMostSpecific allT allH configs SmallcardTitle (IsItem t uuid)
            in
            Item.find allT uuid |> Maybe.map (display mconfig) |> Maybe.withDefault "(missing)"

        HasType t ->
            Type.toPluralString t

        HasUserType t tuid ->
            let
                mconfig =
                    -- we use toHierarchic here because we're displaying a scope, where the tuid is a hierarchic type
                    -- something is not very clear about the notion of scope
                    Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType (Type.toHierarchic t) tuid)

                title =
                    H.find allH tuid |> Maybe.map (display mconfig) |> Maybe.withDefault "(missing)"
            in
            Type.toPluralString t ++ " of type " ++ title

        Identified _ ->
            "Identified"

        And s1 s2 ->
            "(" ++ toDisplay allT allH configs s1 ++ ") And (" ++ toDisplay allT allH configs s2 ++ ")"

        Or s1 s2 ->
            "(" ++ toDisplay allT allH configs s1 ++ ") Or (" ++ toDisplay allT allH configs s2 ++ ")"

        Not s ->
            "Not (" ++ toDisplay allT allH configs s ++ ")"
