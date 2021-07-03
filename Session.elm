module Session exposing (..)

import Browser.Navigation as Nav
import Prng.Uuid exposing (Uuid, generator)
import Random.Pcg.Extended exposing (Seed)
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Time
import Url exposing (Url)

import REA.ProcessType exposing (ProcessType)

type alias Session =
    { currentSeed: Seed
    , currentUuid: Uuid
    , navkey: Nav.Key
    , url: Url
    , processType: ProcessType
    , posixtime: Time.Posix
    }
