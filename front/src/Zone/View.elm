module Zone.View exposing (display, hWithDisplay, tWithDisplay)

import Configuration as Config exposing (Configuration(..))
import Dict exposing (Dict)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Item.Item exposing (Item)
import Prng.Uuid as Uuid
import Scope.Scope exposing (Scope(..))
import Typed.Typed exposing (Typed)
import Zone.Fragment as ZoneFragment
import Zone.Zone as Zone exposing (Zone)


hWithDisplay : Dict String (Typed t) -> Dict String (Hierarchic h) -> Dict String Configuration -> Zone -> Hierarchic h -> Hierarchic h
hWithDisplay allT allH configs zone h =
    let
        mconfig =
            Config.getMostSpecific allT allH configs zone (HasUserType h.what h.uuid {- FIXME h.parent?? -})
    in
    { h | display = Dict.insert (Zone.toString zone) (display mconfig h) h.display }


tWithDisplay : Dict String (Typed t) -> Dict String (Hierarchic h) -> Dict String Configuration -> Zone -> Typed t -> Typed t
tWithDisplay allT allH configs zone t =
    let
        mconfig =
            Config.getMostSpecific allT allH configs zone (HasUserType t.what t.type_)
    in
    { t | display = Dict.insert (Zone.toString zone) (display mconfig t) t.display }


display : Maybe Configuration -> Item i -> String
display mc i =
    mc
        |> Maybe.map (\(ZoneConfig _ fragments _) -> ZoneFragment.display i.identifiers fragments)
        |> Maybe.withDefault (Uuid.toString i.uuid)
