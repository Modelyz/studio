module Ident.Scope exposing (Scope(..))

import Prng.Uuid exposing (Uuid)
import Typed.Type as TType


type
    Scope
    -- This is the scope of an identifier (TODO or a ZoneDisplay)
    -- It allows to assign an identifier to allentities of type T
    -- TODO: AllEntitiesOfGroup or GroupScope Group (Maybe Group) ??
    -- or all the entities of a certain REA type:
    -- Scope could also be used in a group definition? TODO Move in its own module?
    -- all Typed items, possibly of type Uuid
    = TScope TType.Type (Maybe Uuid)
