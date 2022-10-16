module Scope.View exposing (Model, inputScope)

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


type alias Model a =
    { a | scope : Scope }


inputScope : Shared.Model -> (Scope -> msg) -> Model a -> Element msg
inputScope s input model =
    -- TODO replace s with allT allH?
    -- TODO refactor
    let
        allT =
            case model.scope of
                HasType t ->
                    case t of
                        Type.TType tt ->
                            State.allTyped s.state tt
                                |> Dict.map (\_ x -> { x | identifiers = s.state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })

                        Type.HType ht ->
                            State.allTyped s.state (TType.fromHierarchic ht)
                                |> Dict.map (\_ x -> { x | identifiers = s.state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })

                HasUserType t _ ->
                    case t of
                        Type.TType tt ->
                            State.allTyped s.state tt
                                |> Dict.map (\_ x -> { x | identifiers = s.state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })

                        Type.HType ht ->
                            State.allTyped s.state (TType.fromHierarchic ht)
                                |> Dict.map (\_ x -> { x | identifiers = s.state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })

                IsItem (Type.TType tt) uuid ->
                    State.allTyped s.state tt
                        |> Dict.map (\_ x -> { x | identifiers = s.state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })

                _ ->
                    Dict.empty

        allH =
            case model.scope of
                HasType t ->
                    case t of
                        Type.TType tt ->
                            State.allHierarchic s.state (TType.toHierarchic tt)
                                |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })

                        Type.HType ht ->
                            State.allHierarchic s.state ht
                                |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })

                HasUserType t _ ->
                    case t of
                        Type.TType tt ->
                            State.allHierarchic s.state (TType.toHierarchic tt) |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })

                        Type.HType ht ->
                            State.allHierarchic s.state ht |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })

                IsItem (Type.HType ht) uuid ->
                    State.allHierarchic s.state ht
                        |> Dict.map (\_ x -> { x | identifiers = s.state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })

                _ ->
                    Dict.empty
    in
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            [ el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Apply to: "
            , (if model.scope == Empty then
                viewHalfCard Nothing

               else
                viewHalfCard (Just <| input Empty)
              )
                (text <| toDisplay allT allH s.state.configs model.scope)
            ]
        , if model.scope == Empty then
            column [ spacing 10 ]
                [ h2 <| "What should it apply to?"

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
                ]

          else
            none

        -- then the choice of user type, depending on the previously selected concrete type
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            case model.scope of
                HasType (Type.TType tt) ->
                    let
                        -- all the typed items
                        -- all the hierarchic items corresponding to the typed ones
                        allHwithIdentifiers =
                            State.allHierarchic s.state (TType.toHierarchic tt)
                                |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })
                    in
                    (h3 <| "of type:")
                        :: (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (\h -> sClickableCard input allT allHwithIdentifiers s.state.configs h (Type.TType tt))
                           )

                HasType (Type.HType ht) ->
                    -- TODO this should be always be displayed to select a subtype of the hierarchic
                    let
                        allHwithIdentifiers =
                            State.allHierarchic s.state ht
                                |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })
                    in
                    (h3 <| "of type:")
                        :: (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (\h -> sClickableCard input allT allHwithIdentifiers s.state.configs h (Type.HType ht))
                           )

                _ ->
                    []
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            case model.scope of
                HasUserType (Type.TType tt) uuid ->
                    let
                        allTwithIdentifiers =
                            State.allTyped s.state tt |> Dict.map (\_ t -> { t | identifiers = s.state.identifiers |> Dict.filter (\_ id -> t.uuid == id.identifiable) })
                    in
                    (h3 <| "You can select a specific Typed one (otherwise click Next):") :: (allTwithIdentifiers |> Dict.values |> List.map (\t -> tItemClickableCard input allTwithIdentifiers allH s.state.configs t (Type.TType tt)))

                HasUserType (Type.HType ht) uuid ->
                    let
                        allHwithIdentifiers =
                            State.allHierarchic s.state ht |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })
                    in
                    (h3 <| "You can select a specific Hierarchic one (otherwise click Next):") :: (allHwithIdentifiers |> Dict.values |> List.map (\h -> hItemClickableCard input allT allHwithIdentifiers s.state.configs h (Type.HType ht)))

                _ ->
                    []
        ]


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
