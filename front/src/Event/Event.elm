module Event.Event exposing (Event, compare, decoder, encode)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)
import Type exposing (Type)


type alias Event =
    { what : Type
    , uuid : Uuid
    , type_ : Uuid
    , when : Time.Posix
    , identifiers : Dict String Identifier
    , groups : Dict String Group
    , display : Dict String String

    --    , qty: Float
    --    , rtype: ResourceType
    --    , provider: Agent
    --    , receiver: Agent
    }


encode : Event -> Encode.Value
encode e =
    Encode.object <|
        [ ( "what", Type.encode e.what )
        , ( "uuid", Uuid.encode e.uuid )
        , ( "type", Uuid.encode e.type_ )
        , ( "when", Encode.int <| posixToMillis e.when )
        ]


decoder : Decoder Event
decoder =
    Decode.map7 Event
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)


compare : Event -> String
compare =
    .uuid >> Uuid.toString
