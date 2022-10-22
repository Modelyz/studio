module Zone.View exposing (hDisplay, hWithDisplay, tDisplay, tWithDisplay)

import Configuration as Config exposing (Configuration(..))
import Dict exposing (Dict)
import Group.Group exposing (Group)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Item.Item exposing (Item)
import Prng.Uuid as Uuid
import Scope.Scope exposing (Scope(..))
import Typed.Typed exposing (Typed)
import Zone.Fragment as ZoneFragment
import Zone.Zone as Zone exposing (Zone)


gDisplay : Dict String (Typed a) -> Dict String (Hierarchic h) -> Dict String Configuration -> Zone -> Group -> String
gDisplay allT allH configs zone t =
    Config.getMostSpecific allT allH configs zone (HasUserType t.what t.type_)
        |> Maybe.map (\(ZoneConfig _ fragments _) -> ZoneFragment.display t.identifiers fragments)
        |> Maybe.withDefault (Uuid.toString t.uuid)


tDisplay : Dict String (Typed a) -> Dict String (Hierarchic h) -> Dict String Configuration -> Zone -> Typed a -> String
tDisplay allT allH configs zone t =
    Config.getMostSpecific allT allH configs zone (HasUserType t.what t.type_)
        |> Maybe.map (\(ZoneConfig _ fragments _) -> ZoneFragment.display t.identifiers fragments)
        |> Maybe.withDefault (Uuid.toString t.uuid)


hDisplay : Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Zone -> Hierarchic b -> String
hDisplay allT allH configs zone h =
    -- return the display string of the hierarchic item
    Config.getMostSpecific allT allH configs zone (HasUserType h.what h.uuid {- FIXME h.parent ?? like t.type_ above? -})
        |> Maybe.map (\(ZoneConfig _ fragments _) -> ZoneFragment.display h.identifiers fragments)
        |> Maybe.withDefault (Uuid.toString h.uuid)


hWithDisplay : Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Zone -> Hierarchic b -> Hierarchic b
hWithDisplay allT allH configs zone h =
    { h | display = Dict.insert (Zone.toString zone) (hDisplay allT allH configs zone h) h.display }


tWithDisplay : Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Zone -> Typed a -> Typed a
tWithDisplay allT allH configs zone t =
    { t | display = Dict.insert (Zone.toString zone) (tDisplay allT allH configs zone t) t.display }
