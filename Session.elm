module Session exposing (..)

import Browser.Navigation as Nav
import Prng.Uuid exposing (Uuid, generator)
import REA.ProcessType exposing (ProcessType)
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Time
import Url exposing (Url)


type alias Session =
    { currentSeed : Seed
    , currentUuid : Uuid
    , navkey : Nav.Key
    , url : Url
    , processType : ProcessType
    }
