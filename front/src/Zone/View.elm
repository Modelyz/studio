module Zone.View exposing (hDisplay, hWithDisplay, tDisplay, tWithDisplay)

import Configuration as Config exposing (Configuration(..))
import Dict exposing (Dict)
import Group.Group exposing (Group)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Hierarchy.Type as HType
import Item.Item exposing (Item)
import Prng.Uuid as Uuid
import Scope.Scope exposing (Scope(..))
import Type
import Typed.Type as TType
import Typed.Typed exposing (Typed)
import Zone.Fragment as ZoneFragment
import Zone.Zone as Zone exposing (Zone)


gDisplay : Dict String (Typed a) -> Dict String (Hierarchic h) -> Dict String Configuration -> Zone -> Group -> String
gDisplay allT allH configs zone g =
    Config.getMostSpecific allT allH configs zone (HasUserType HType.GroupType g.uuid)
        |> Maybe.map (\(ZoneConfig _ fragments _) -> ZoneFragment.display g.identifiers fragments)
        |> Maybe.withDefault (Uuid.toString g.uuid)


tDisplay : Dict String (Typed a) -> Dict String (Hierarchic h) -> Dict String Configuration -> Zone -> Typed a -> String
tDisplay allT allH configs zone t =
    Debug.log "tDisplay Config.getMostSpecific" (Config.getMostSpecific allT allH configs zone (IsItem (Type.TType t.what) t.uuid))
        |> Maybe.map (\(ZoneConfig _ fragments _) -> ZoneFragment.display t.identifiers fragments)
        |> Maybe.withDefault (Uuid.toString t.uuid)


hDisplay : Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Zone -> Hierarchic b -> String
hDisplay allT allH configs zone h =
    -- return the display string of the hierarchic item
    Debug.log "hDisplay Config.getMostSpecific" (Config.getMostSpecific allT allH configs zone (IsItem (Type.HType h.what) h.uuid))
        |> Maybe.map (\(ZoneConfig _ fragments _) -> ZoneFragment.display h.identifiers fragments)
        |> Maybe.withDefault (Uuid.toString h.uuid)


hWithDisplay : Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Zone -> Hierarchic b -> Hierarchic b
hWithDisplay allT allH configs zone h =
    { h | display = Dict.insert (Zone.toString zone) (hDisplay allT allH configs zone h) h.display }


tWithDisplay : Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Zone -> Typed a -> Typed a
tWithDisplay allT allH configs zone t =
    { t | display = Dict.insert (Zone.toString zone) (tDisplay allT allH configs zone t) t.display }
