module Scope.View exposing (inputScope)

import Configuration as Config exposing (Configuration(..))
import Dict exposing (Dict)
import Effect as Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Entity.Entity as Entity
import Hierarchy.Hierarchic as Hierarchic exposing (Hierarchic)
import Hierarchy.Type as HType
import Ident.Identifiable as Identifiable exposing (Identifiable, hWithIdentifiers, tWithIdentifiers)
import Ident.Identification as Identification
import Ident.Identifier as Identifier
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Ident.View
import Item.Item as Item exposing (Item)
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect, redirectParent)
import Scope.Scope as Scope exposing (Scope(..))
import Shared
import Spa.Page
import State
import Type exposing (Type(..))
import Typed.Type as TType
import Typed.Typed as Typed exposing (Typed)
import View exposing (..)
import View.Lang exposing (Lang(..))
import View.Smallcard exposing (sClickableCard, viewHalfCard)
import View.Style exposing (..)
import Zone.Fragment as Fragment exposing (Fragment(..))
import Zone.Zone as Zone exposing (Zone(..))


type alias Model a =
    { a | scope : Scope }


inputScope : Shared.Model -> (Scope -> msg) -> Model a -> Element msg
inputScope s input model =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Apply to: ")
                :: [ viewHalfCard (input Empty) (text <| Scope.toString model.scope) ]
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
                        allT =
                            State.allTyped s.state tt

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
                                |> List.map (\h -> sClickableCard input allT allHwithIdentifiers s.state.configs h)
                           )

                HasType (Type.HType ht) ->
                    let
                        allT =
                            Dict.empty

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
                                |> List.map (\h -> sClickableCard input allT allHwithIdentifiers s.state.configs h)
                           )

                _ ->
                    []
        ]
