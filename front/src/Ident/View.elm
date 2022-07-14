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


displayIdentifier : Identifiable -> List Identifier -> Element msg
displayIdentifier identifiable identifiers =
    -- display the first identifier, which may depend on others in the list
    identifiers
        |> List.head
        |> Maybe.map
            (\identifier ->
                row [ spacing 5 ] <|
                    List.map (\f -> displayFragment f identifiable identifiers) identifier.fragments
            )
        |> Maybe.withDefault (text <| Identifiable.toString identifiable)


selectIdentifier : Identifiable -> ViewType -> List Identifier -> List Identifier
selectIdentifier identifiable vt identifiers =
    -- TODO make "Display name" configurable against the view type and the identifiable
    case vt of
        Smallcard ->
            identifiers
                |> List.sortBy
                    (\i ->
                        if i.name == "Display name" then
                            0

                        else
                            1
                    )

        New ->
            []


displayFragment : Fragment -> Identifiable -> List Identifier -> Element msg
displayFragment fragment identifiable identifiers =
    case fragment of
        Free value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text value ]

        Fixed value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text value ]

        OtherIdentifier name ->
            identifiers
                |> List.filter (\i -> i.name == name)
                |> displayIdentifier identifiable

        Sequence padding step start value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text <| Maybe.withDefault "(Not yet assigned)" value ]

        _ ->
            text "other"
