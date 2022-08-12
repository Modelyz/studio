module Scope.View exposing (inputScope)

import Configuration exposing (Configuration(..))
import Dict exposing (Dict)
import Effect as Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Entity.Entity as Entity
import Hierarchy.Hierarchic as Hierarchic
import Hierarchy.Type as HType
import Ident.Identifiable exposing (hWithIdentifiers, tWithIdentifiers)
import Ident.Identification as Identification
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Ident.View
import Item.Item as Item exposing (Item)
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect, redirectParent)
import Scope.Scope as Scope exposing (Scope(..), toString)
import Shared
import Spa.Page
import State
import Type exposing (Type(..))
import Typed.Type as TType
import Typed.Typed as Typed exposing (Typed)
import View exposing (..)
import View.Lang exposing (Lang(..))
import View.Style exposing (..)
import Zone.Fragment as Fragment exposing (Fragment(..))
import Zone.Zone as Zone exposing (Zone(..))


type alias Model a =
    { a | scope : Scope }


removableCard : (Scope -> msg) -> Scope -> Element msg
removableCard input scope =
    -- TODO duplicated from Ident/AddPage
    row [ Background.color color.item.background ]
        [ el [ paddingXY 10 2 ] (text <| toString scope)
        , button.primary (input Empty) "×"
        ]


inputScope : Shared.Model -> (Scope -> msg) -> Model a -> Element msg
inputScope s input model =
    -- TODO duplicated from Ident/AddPage → refactor
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Apply to: ")
                :: [ removableCard input model.scope ]
        , h2 <| "Select the types of the entities you want to configure the zone for"

        -- First the general types
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (TType.all
                |> List.map
                    (\t ->
                        clickableCard (input (IsType (Type.TType t))) (text <| "All " ++ TType.toPluralString t) none
                    )
            )
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (HType.all
                |> List.map
                    (\t ->
                        clickableCard (input (IsType (Type.HType t))) (text <| "All " ++ HType.toPluralString t) none
                    )
            )

        -- then the choice of user types, depending on the previously selected general type
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            case model.scope of
                IsType (Type.TType tt) ->
                    let
                        allTyped =
                            State.allTyped s.state tt

                        allHierarchic =
                            State.allHierarchic s.state (TType.toHierarchic tt)

                        config =
                            Configuration.getMostSpecific allTyped allHierarchic s.state.configs SmallcardItemTitle model.scope

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
                    allHierarchic
                        |> hWithIdentifiers s.state.identifiers
                        |> Dict.values
                        |> List.filter
                            (\i -> Scope.containsScope allTyped allHierarchic configscope (HasUserType i.uuid))
                        |> List.map
                            (\t ->
                                clickableCard
                                    (input <| And model.scope (HItem t.uuid))
                                    (text <| toString (HItem t.uuid))
                                    (t.parent |> Maybe.map (Uuid.toString >> text) |> Maybe.withDefault none)
                            )

                IsType (Type.HType ht) ->
                    let
                        allTyped =
                            State.allTyped s.state (TType.fromHierarchic ht)

                        allHierarchic =
                            State.allHierarchic s.state ht

                        config =
                            Configuration.getMostSpecific allTyped allHierarchic s.state.configs SmallcardItemTitle model.scope

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
                    allHierarchic
                        |> hWithIdentifiers s.state.identifiers
                        |> Dict.values
                        |> List.filter
                            (\i -> Scope.containsScope allTyped allHierarchic configscope (HasUserType i.uuid))
                        |> List.map
                            (\t ->
                                clickableCard
                                    (input <| And model.scope (HItem t.uuid))
                                    (text <| toString (HItem t.uuid))
                                    (t.parent |> Maybe.map (Uuid.toString >> text) |> Maybe.withDefault none)
                            )

                _ ->
                    []
        ]
