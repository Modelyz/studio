module View.DisplayIdentifiers exposing (..)

import DictSet as Set exposing (DictSet)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import REA.Entity exposing (Entity(..))
import REA.Ident exposing (EntityIdentifier, Fragment(..), Identifiable(..), Identifier, Name, fragmentToName, fragmentToString, fragmentToValue, getEntityIdentifier, getIdentifier, identifierValue, updateIdentifier)
import Style exposing (size)
import View exposing (ViewType(..))


type alias Model a =
    { a | identifiers : DictSet String EntityIdentifier }


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
                    getEntityIdentifier (Agent a) "Display name" model.identifiers

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
                    getEntityIdentifier e name model.identifiers
                        |> Maybe.map .identifier
                        |> Maybe.map (\i -> String.join " " <| List.map fragmentToValue i.fragments)
                        |> Maybe.withDefault ("(Error in this identifier: " ++ name ++ " does not exist)")
                        |> text

                _ ->
                    text "Not Implemented"

        Sequence padding step start value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text <| Maybe.withDefault "(Not yet assigned)" value ]

        _ ->
            text "other"
