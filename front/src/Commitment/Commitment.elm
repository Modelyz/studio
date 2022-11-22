module Commitment.Commitment exposing (Commitment, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)
import Typed.Type as TType


type alias Commitment =
    { what : TType.Type
    , uuid : Uuid
    , type_ : Uuid
    , when : Time.Posix

    {- , provider : Uuid
       , receiver : Uuid
       , flow : Flow
    -}
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
            {- provider receiver flow -}
            Commitment what uuid type_ when
         {- provider receiver flow -}
        )
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))



{- (Decode.field "provider" Uuid.decoder)
   (Decode.field "receiver" Uuid.decoder)
   (Decode.field "flow" Flow.decoder)
-}
