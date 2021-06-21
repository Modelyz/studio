module ES exposing (..)

import Prng.Uuid
import Json.Encode
import Time

type alias Event =
    { uuid: Prng.Uuid.Uuid
    , posixtime: Time.Posix
    , name: String
    }


encode : Event -> Json.Encode.Value
encode event =
    Json.Encode.object
        [ ("uuid", Json.Encode.string <| Prng.Uuid.toString event.uuid)
        , ("posixtime", Json.Encode.int <| Time.posixToMillis event.posixtime)
        ,  ("name", Json.Encode.string event.name)]


    
    
