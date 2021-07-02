module ES exposing (..)

import Prng.Uuid
import Json.Encode
import Json.Decode exposing (andThen)
import Time
import REA.Entity as En exposing (Entity)


-- business events --
type alias Event =
    { uuid: Prng.Uuid.Uuid
    , posixtime: Time.Posix
    , name: String
    , entity: Entity
    , entityType: String
    }


encode : Event -> Json.Encode.Value
encode event =
    Json.Encode.object
        [ ("uuid", Prng.Uuid.encode event.uuid)
        , ("posixtime", Json.Encode.int <| Time.posixToMillis event.posixtime)
        , ("name", Json.Encode.string event.name)
        , ("entityType", Json.Encode.string event.entityType)
        , ("entity", En.encode event.entity)
        ]


decode : Json.Decode.Decoder Event
decode =
    Json.Decode.map5 Event
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "posixtime" Json.Decode.int |> andThen intToPosix)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "entityType" Json.Decode.string
            |> andThen En.decode
        )
        (Json.Decode.field "entityType" Json.Decode.string)

intToPosix : Int -> Json.Decode.Decoder Time.Posix
intToPosix millis = Json.Decode.succeed <| Time.millisToPosix millis


