module AgentType.AgentType exposing (AgentType, compare, decoder, encode, toString)

import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)


type alias AgentType =
    { uuid : Uuid
    , type_ : Maybe Uuid
    , group : Maybe Uuid
    }


encode : AgentType -> Encode.Value
encode at =
    Encode.object <|
        [ ( "uuid", Uuid.encode at.uuid )
        , ( "type", Maybe.map Uuid.encode at.type_ |> Maybe.withDefault Encode.null )
        ]
            ++ (Maybe.map (\g -> [ ( "group", Uuid.encode g ) ]) at.group |> Maybe.withDefault [])


decoder : Decode.Decoder AgentType
decoder =
    Decode.map3 AgentType
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" <| Decode.maybe Uuid.decoder)
        (Decode.maybe (Decode.field "group" Uuid.decoder))


compare : AgentType -> String
compare =
    toString


toString : AgentType -> String
toString =
    .uuid >> Uuid.toString
