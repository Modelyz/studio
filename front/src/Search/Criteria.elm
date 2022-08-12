module Search.Criteria exposing (Criteria(..))

import Dict exposing (Dict)
import Ident.Identifiable as Identifiable exposing (Identifiable)
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)


type Criteria a
    = SearchNothing
    | SearchFull String
      -- TODO try not to depend on Ident
    | SearchIdentifier IdentifierType String



-- pour full : d'abord on cherche les identifiertypes, puis on filtre les EntityIdentifier selon la chaîne de recherche et on affiche les entities
--entitySearch :
--    Criteria
--    -> Dict String IdentifierType
--    -> Dict String EntityType
--    -> Dict String EntityIdentifier
--    -> Dict String Entity
--    -> Dict String Entity
--entitySearch criteria identifierTypes entityTypes entityIdentifiers entities =
--    -- return the set of entities that match the criteria (given the catalog of entities and identifierTypes)
--    case criteria of
--        SearchNothing ->
--            entities
--
--        SearchFull identifiable string ->
--            -- first get the relevant identifierTypes
--            let
--                its =
--                    identifierTypes
--                    |> Dict.filter (\_ it -> IdentifierType.within it entityTypes identifiable)
--                eis =
--                    entityIdentifiers
--                    |> EntityIdentifier.restrict
--                    |> Dict.map Identifier.compare .identifier |>Dict.filter (\_ i
--            in
--            case identifiable of
--                Identifiable.Entity entity ->
---                    entities
--                        |> (\e -> EntityIdentifier.restrict e entities)
--                        |> Dict.map .identifiable
--                        |> List.filter (\i -> Identifiable.toString i == Entity.toString entity)
--
--                _ ->
--                    Set.empty
--
--        _ ->
--            -- TODO
--            Debug.todo "SearchIdentifier not implemented"
