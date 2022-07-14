module Ident.View exposing (..)

import DictSet as Set exposing (DictSet)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Ident.EntityIdentifier as EntityIdentifier exposing (EntityIdentifier)
import Ident.Fragment as Fragment exposing (Fragment(..))
import Ident.Identifiable as Identifiable exposing (Identifiable(..))
import Ident.Identifier as Identifier exposing (Identifier)
import REA.Entity exposing (Entity(..))
import Style exposing (size)
import View exposing (ViewType(..))


type alias Model a =
    { a | identifiers : DictSet String EntityIdentifier }


type alias Config msg =
    { onEnter : msg
    , onInput : Identifier -> msg
    }


displayIdentifier : Model a -> EntityIdentifier -> Element msg
displayIdentifier model ei =
    let
        id =
            ei.identifier

        ie =
            ei.identifiable
    in
    row [ spacing 5 ] <|
        List.map (\f -> displayFragment model ie f id) id.fragments


defaultIdentifier : Model a -> Entity -> ViewType -> Maybe EntityIdentifier
defaultIdentifier model ent vt =
    case ent of
        Agent a ->
            case vt of
                Smallcard ->
                    EntityIdentifier.select (Agent a) "Display name" model.identifiers

                New ->
                    Nothing

        _ ->
            Nothing


displayFragment : Model a -> Identifiable -> Fragment -> Identifier -> Element msg
displayFragment model ie fragment ident =
    case fragment of
        Free value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text value ]

        Fixed value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text value ]

        OtherIdentifier name ->
            case ie of
                Entity e ->
                    EntityIdentifier.select e name model.identifiers
                        |> Maybe.map .identifier
                        |> Maybe.map (\i -> String.join " " <| List.map Fragment.toValue i.fragments)
                        |> Maybe.withDefault ("(Error in this identifier: " ++ name ++ " does not exist)")
                        |> text

                _ ->
                    text "Not Implemented"

        Sequence padding step start value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text <| Maybe.withDefault "(Not yet assigned)" value ]

        _ ->
            text "other"


inputIdentifiers : Config msg -> Model a -> Element msg
inputIdentifiers c model =
    -- display an input field for each relevant identifier
    column [ spacing 10 ]
        (model.identifiers
            |> Set.toList
            |> List.map
                (\i -> inputIdentifier c model i)
        )


inputIdentifier : Config msg -> Model a -> Identifier -> Element msg
inputIdentifier c model id =
    column []
        [ el [ paddingXY 0 10 ] <| text id.name
        , row [ spacing 5 ] <|
            List.indexedMap (\i f -> inputFragment c model i f id) id.fragments
        ]


inputFragment : Config msg -> Model a -> Int -> Fragment -> Identifier -> Element msg
inputFragment c model index fragment ident =
    case fragment of
        Free value ->
            Input.text
                [ width <| minimum 200 fill
                , Input.focusedOnLoad
                , View.onEnter c.onEnter
                ]
                { onChange = \v -> c.onInput <| Identifier.update index (Free v) ident
                , text = value
                , placeholder =
                    Just <| Input.placeholder [] <| text ident.name
                , label = Input.labelHidden ident.name
                }

        Fixed value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text value ]

        OtherIdentifier name ->
            Identifier.select name model.identifiers
                |> Maybe.map (\i -> String.join " " <| List.map Fragment.toValue i.fragments)
                |> Maybe.withDefault ("(Error in this identifier: " ++ name ++ " does not exist)")
                |> text

        Sequence padding step start value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text <| Maybe.withDefault "(Not yet assigned)" value ]

        _ ->
            text "Not Implemented"
