module Ident.View exposing (..)

import DictSet as Set exposing (DictSet)
import Element exposing (..)
import Entity.Entity as Entity exposing (Entity)
import Ident.Fragment as Fragment exposing (Fragment(..))
import Ident.Identifier as Identifier exposing (Identifier)
import View exposing (withDefaultContent)
import View.Type as ViewType exposing (Type(..))


type alias Model a =
    { a | identifiers : DictSet String Identifier }


type alias Config msg =
    { onEnter : msg
    , onInput : Identifier -> msg
    }


displayIdentifiers : String -> List Identifier -> Element msg
displayIdentifiers default identifiers =
    -- display the first identifier, which may depend on others in the list
    row [ spacing 5 ]
        (identifiers
            |> List.map
                (\identifier ->
                    row [ spacing 5 ] <|
                        List.map (\f -> displayFragment f identifiers) identifier.fragments
                )
            |> withDefaultContent (text default)
        )


buildDisplayIdentifier : ViewType.Type -> Entity -> List Identifier -> List Identifier
buildDisplayIdentifier viewtype e identifiers =
    -- TODO make this configurable against the view type and the identifiable
    -- il faut retrouver la configuration de display identifier en partant du plus spécifique au moins spécifique
    -- construire la liste des scopes parents de e jusqu'au scope racine (AllEntities)
    -- pour chaque scope de la liste, chercher la configuration correspondante.
    -- récupérer le nom des identifiants
    -- récupérer les identifiants
    -- afficher le display identifier
    case viewtype of
        Smallcard ->
            (identifiers |> List.filter (\i -> i.name == "NCODE"))
                ++ (identifiers |> List.filter (\i -> i.name == "XXXPrénom"))
                ++ (identifiers |> List.filter (\i -> i.name == "XXXName"))
                ++ (identifiers |> List.filter (\i -> i.name == "Nom"))

        New ->
            []


displayFragment : Fragment -> List Identifier -> Element msg
displayFragment fragment identifiers =
    case fragment of
        Free value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text value ]

        Fixed value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text value ]

        Sequence padding step start value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text <| Maybe.withDefault "(Not yet assigned)" value ]

        _ ->
            text "other"
