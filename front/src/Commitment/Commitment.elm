module Commitment.Commitment exposing (Commitment, compare, decoder, encode)

import Agent.Agent as Agent exposing (Agent)
import Dict exposing (Dict)
import Group.Group exposing (Group)
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)
import Type exposing (Type)
import Typed.Type as TType
import Value.Rational as Rational exposing (Rational(..))
import Value.Value exposing (Value)


type alias Commitment =
    { what : TType.Type
    , uuid : Uuid
    , type_ : Uuid
    , when : Time.Posix
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , groups : Dict String Group
    , display : Dict String String

    --, qty : Rational
    --        , rtype: ResourceType
    --, provider : Uuid
    --, receiver : Uuid
    }


encode : Commitment -> Encode.Value
encode c =
    Encode.object <|
        [ ( "what", TType.encode c.what )
        , ( "uuid", Uuid.encode c.uuid )
        , ( "type", Uuid.encode c.type_ )
        , ( "when", Encode.int <| posixToMillis c.when )
        ]


decoder : Decode.Decoder Commitment
decoder =
    Decode.map4
        (\what uuid type_ when ->
            {- qty provider receiver -}
            Commitment what uuid type_ when Dict.empty Dict.empty Dict.empty Dict.empty
         --qty provider receiver
        )
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))



--(Decode.field "qty" Rational.decoder)
--(Decode.field "provider" Uuid.decoder)
--(Decode.field "receiver" Uuid.decoder)


compare : Commitment -> String
compare =
    .uuid >> Uuid.toString
