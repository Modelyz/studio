module Scope.View exposing (inputScope, toDisplay)

import Configuration as Config exposing (Configuration(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Hierarchy.Type as HType
import Ident.Identifiable as Identifiable exposing (Identifiable, hWithIdentifiers, tWithIdentifiers)
import Item.Item as Item exposing (Item)
import Scope.Scope as Scope exposing (Scope(..))
import Shared
import State
import Type exposing (Type(..))
import Typed.Type as TType
import Typed.Typed as Typed exposing (Typed)
import View exposing (..)
import View.Smallcard exposing (clickableCard, sClickableCard, viewHalfCard)
import View.Style exposing (..)
import Zone.View exposing (display)
import Zone.Zone as Zone exposing (Zone(..))


type alias Model a =
    { a | scope : Scope }


inputScope : Shared.Model -> (Scope -> msg) -> Model a -> Element msg
inputScope s input model =
    -- TODO replace s with allT allH
    let
        allT =
            case model.scope of
                HasType t ->
                    case t of
                        Type.TType tt ->
                            tWithIdentifiers s.state.identifiers <| State.allTyped s.state tt

                        Type.HType ht ->
                            tWithIdentifiers s.state.identifiers <| State.allTyped s.state (TType.fromHierarchic ht)

                HasUserType t _ ->
                    case t of
                        Type.TType tt ->
                            tWithIdentifiers s.state.identifiers <| State.allTyped s.state tt

                        Type.HType ht ->
                            tWithIdentifiers s.state.identifiers <| State.allTyped s.state (TType.fromHierarchic ht)

                _ ->
                    Dict.empty

        allH =
            case model.scope of
                HasType t ->
                    case t of
                        Type.TType tt ->
                            hWithIdentifiers s.state.identifiers <| State.allHierarchic s.state (TType.toHierarchic tt)

                        Type.HType ht ->
                            hWithIdentifiers s.state.identifiers <| State.allHierarchic s.state ht

                HasUserType t _ ->
                    case t of
                        Type.TType tt ->
                            hWithIdentifiers s.state.identifiers <| State.allHierarchic s.state (TType.toHierarchic tt)

                        Type.HType ht ->
                            hWithIdentifiers s.state.identifiers <| State.allHierarchic s.state ht

                _ ->
                    Dict.empty
    in
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Apply to: ")
                :: [ viewHalfCard (input Empty) (text <| toDisplay allT allH s.state.configs model.scope) ]
        , h2 <| "What should it apply to?"

        -- First the concrete types
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (TType.all
                |> List.map
                    (\t ->
                        clickableCard (input (HasType (Type.TType t))) (text <| TType.toPluralString t) none
                    )
            )
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (HType.all
                |> List.map
                    (\t ->
                        clickableCard (input (HasType (Type.HType t))) (text <| HType.toPluralString t) none
                    )
            )

        -- then the choice of user type, depending on the previously selected concrete type
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            case model.scope of
                HasType (Type.TType tt) ->
                    let
                        -- all the typed items
                        -- all the hierarchic items corresponding to the typed ones
                        allHwithIdentifiers =
                            hWithIdentifiers s.state.identifiers <| State.allHierarchic s.state (TType.toHierarchic tt)

                        config =
                            Config.getMostSpecific allT allHwithIdentifiers s.state.configs SmallcardTitle model.scope

                        configscope =
                            Maybe.map
                                (\c ->
                                    case c of
                                        ZoneConfig _ _ scope ->
                                            scope
                                )
                                config
                                |> Maybe.withDefault Empty
                    in
                    (h2 <| "And more precisely:")
                        :: (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (\h -> sClickableCard input allT allHwithIdentifiers s.state.configs h (Type.TType tt))
                           )

                HasType (Type.HType ht) ->
                    let
                        allHwithIdentifiers =
                            hWithIdentifiers s.state.identifiers <| State.allHierarchic s.state ht

                        config =
                            Config.getMostSpecific allT allHwithIdentifiers s.state.configs SmallcardTitle model.scope

                        configscope =
                            Maybe.map
                                (\c ->
                                    case c of
                                        ZoneConfig _ _ scope ->
                                            scope
                                )
                                config
                                |> Maybe.withDefault Empty
                    in
                    (h2 <| "And more precisely:")
                        :: (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (\h -> sClickableCard input allT allHwithIdentifiers s.state.configs h (Type.HType ht))
                           )

                _ ->
                    []
        ]


toDisplay : Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> Scope -> String
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
            Type.toString t

        HasUserType t tuid ->
            let
                mconfig =
                    -- we use toHierarchic here because we're displaying a scope, where the tuid is a hierarchic type
                    -- something is not very clear about the notion of scope
                    Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType (Type.toHierarchic t) tuid)

                title =
                    H.find allH tuid |> Maybe.map (display mconfig) |> Maybe.withDefault "(missing)"
            in
            Type.toString t ++ " of type " ++ title

        Identified _ ->
            "Identified"

        And s1 s2 ->
            "(" ++ toDisplay allT allH configs s1 ++ ") And (" ++ toDisplay allT allH configs s2 ++ ")"

        Or s1 s2 ->
            "(" ++ toDisplay allT allH configs s1 ++ ") Or (" ++ toDisplay allT allH configs s2 ++ ")"

        Not s ->
            "Not (" ++ toDisplay allT allH configs s ++ ")"
