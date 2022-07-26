module Ident.View exposing (..)

import Configuration exposing (Configuration(..))
import DictSet as Set exposing (DictSet)
import Element exposing (..)
import Entity.Entity as Entity exposing (Entity)
import Entity.Type as EntityType exposing (Type(..))
import Ident.Fragment as IdentFragment
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.Scope as Scope exposing (Scope(..))
import Shared
import View exposing (withDefaultContent)
import View.Lang as Lang exposing (Lang(..))
import View.Type as ViewType
import Zone.Fragment as ZoneFragment
import Zone.Zone as Zone exposing (Zone(..))


type alias Model a =
    { a | identifiers : DictSet String Identifier }


type alias Config msg =
    { onEnter : msg
    , onInput : Identifier -> msg
    }


display : Shared.Model -> Zone -> Lang -> Entity -> Element msg
display s zone lang entity =
    s.state.identifiers
        |> Identifier.fromEntity entity
        |> buildDisplayIdentifier s ViewType.Smallcard entity
        |> displayIdentifiers (Entity.toUuidString entity)


buildDisplayIdentifier : Shared.Model -> ViewType.Type -> Entity -> DictSet String Identifier -> List Identifier
buildDisplayIdentifier s viewtype e identifiers =
    let
        scope =
            Scope.fromEntity e

        parents =
            Scope.getParentsToRoot scope s.state.entities []

        firstRelevant =
            Configuration.findFirst s.state.configs parents

        fragments =
            firstRelevant |> Maybe.map (\(ZoneConfig _ fs _) -> fs) |> Maybe.withDefault []

        fragmentStrings =
            ZoneFragment.display identifiers fragments

        entityIdentifiers =
            Identifier.fromEntity e identifiers
    in
    case viewtype of
        ViewType.Smallcard ->
            fragments
                |> List.map
                    (\fragment ->
                        case fragment of
                            ZoneFragment.Fixed string ->
                                [ Identifier (Entity.toUuid e) "Separator" [ IdentFragment.Fixed string ] ]

                            ZoneFragment.IdentifierName name ->
                                entityIdentifiers |> Set.filter (\i -> i.name == name) |> Set.toList
                    )
                |> List.concat

        ViewType.New ->
            []


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


displayFragment : IdentFragment.Fragment -> List Identifier -> Element msg
displayFragment fragment identifiers =
    case fragment of
        IdentFragment.Free value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text value ]

        IdentFragment.Fixed value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text value ]

        IdentFragment.Sequence padding step start value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text <| Maybe.withDefault "(Not yet assigned)" value ]

        _ ->
            text "other"


displayScope : Shared.Model -> Scope -> Element msg
displayScope s id =
    -- TODO refactor to avoid Element here
    case id of
        AllEntities type_ ->
            text <| EntityType.toString type_

        AllEntitiesOfType type_ uuid ->
            row []
                [ text <| EntityType.toString type_ ++ " of type "
                , Entity.fromUuid s.state.entities uuid
                    |> Maybe.map (display s SmallcardItemTitle FR_fr)
                    |> Maybe.withDefault (text "plop")
                ]
