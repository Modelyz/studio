module Search.Criteria exposing (Criteria(..))

import DictSet as Set exposing (DictSet)
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
--    -> DictSet String IdentifierType
--    -> DictSet String EntityType
--    -> DictSet String EntityIdentifier
--    -> DictSet String Entity
--    -> DictSet String Entity
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
--                    |> Set.filter (\it -> IdentifierType.within it entityTypes identifiable)
--                eis =
--                    entityIdentifiers
--                    |> EntityIdentifier.restrict
--                    |> Set.map Identifier.compare .identifier |>Set.filter (\i
--            in
--            case identifiable of
--                Identifiable.Entity entity ->
---                    entities
--                        |> (\e -> EntityIdentifier.restrict e entities)
--                        |> Set.map .identifiable
--                        |> List.filter (\i -> Identifiable.toString i == Entity.toString entity)
--
--                _ ->
--                    Set.empty
--
--        _ ->
--            -- TODO
--            Debug.todo "SearchIdentifier not implemented"
