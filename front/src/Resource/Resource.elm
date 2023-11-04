module Resource.Resource exposing (Resource, compare, decoder, encode, resourceOf)

import Dict exposing (Dict)
import Expression.Rational as Rational exposing (Rational)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Scope exposing (Scope(..))
import Typed.Type as TType


type alias Resource =
    { what : TType.Type
    , uuid : Uuid -- not sure we always need a uuid ?
    , type_ : Uuid

    -- for one resource of 1,5 water liter or one transfert of 15 EUR or one lot of 15 identical pens
    , qty : Rational
    }


compare : Resource -> String
compare =
    -- what allows to distinguish two resources?
    -- currently it's the uuid.
    -- Maybe in the future we can allow the identifiers to play that role
    .uuid >> Uuid.toString


resourceOf : Dict String { a | resource : Uuid } -> Dict String Resource -> Uuid -> List Uuid
resourceOf entities resources uuid =
    Dict.get (Uuid.toString uuid) entities
        |> Maybe.map
            (\e ->
                resources
                    |> Dict.filter (\_ v -> v.uuid == e.resource)
                    |> Dict.values
                    |> List.map .uuid
            )
        |> Maybe.withDefault []


encode : Resource -> Encode.Value
encode r =
    Encode.object <|
        [ ( "what", TType.encode r.what )
        , ( "uuid", Uuid.encode r.uuid )
        , ( "type_", Uuid.encode r.type_ )
        , ( "qty", Rational.encode r.qty )
        ]


decoder : Decoder Resource
decoder =
    Decode.map4 Resource
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type_" Uuid.decoder)
        (Decode.field "qty" Rational.decoder)
