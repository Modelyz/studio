module Session exposing (Session)

import Browser.Navigation as Nav
import Prng.Uuid exposing (Uuid)
import REA.ProcessType exposing (ProcessType)
import Random.Pcg.Extended exposing (Seed)
import Url exposing (Url)


type alias Session =
    { currentSeed : Seed
    , currentUuid : Uuid
    , navkey : Nav.Key
    , url : Url
    , processType : ProcessType
    }
