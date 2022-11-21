module Zone.View exposing (display)

import Configuration as Config exposing (Configuration(..))
import Dict exposing (Dict)
import Group.Group exposing (Group)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Hierarchy.Type as HType
import Ident.Identifier exposing (Identifier)
import Item.Item exposing (Item)
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope exposing (Scope(..))
import Type exposing (Type)
import Typed.Type as TType
import Typed.Typed exposing (Typed)
import Zone.Fragment as ZoneFragment
import Zone.Zone as Zone exposing (Zone)


display : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String Configuration -> Zone -> Dict String Identifier -> Type -> Uuid -> String
display types configs zone identifiers t uuid =
    Config.getMostSpecific types configs zone (IsItem t uuid)
        |> Maybe.map (\(ZoneConfig _ fragments _) -> ZoneFragment.display identifiers fragments)
        |> Maybe.withDefault (Uuid.toString uuid)
