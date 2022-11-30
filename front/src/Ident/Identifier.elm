module Ident.Identifier exposing (Identifier, compare, decoder, encode, fromUuid, select, toValue, update)

import Dict exposing (Dict)
import Ident.Fragment as Fragment exposing (Fragment)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias Identifier =
    -- This is the value of an identifier
    { what : Type
    , identifiable : Uuid -- TODO rename to 'for'
    , name : String
    , fragments : List Fragment
    }


select : String -> Dict String Identifier -> List Identifier
select name =
    Dict.filter (\_ i -> i.name == name) >> Dict.values


fromUuid : Uuid -> Dict String Identifier -> Dict String Identifier
fromUuid uuid =
    Dict.filter (\_ i -> uuid == i.identifiable)


compare : Identifier -> String
compare i =
    Type.compare i.what ++ "/" ++ Uuid.toString i.identifiable ++ "/" ++ i.name


encode : Identifier -> Encode.Value
encode i =
    Encode.object
        [ ( "what", Type.encode i.what )
        , ( "identifiable", Uuid.encode i.identifiable )
        , ( "name", Encode.string i.name )
        , ( "fragments", Encode.list Fragment.encode i.fragments )
        ]


decoder : Decoder Identifier
decoder =
    Decode.map4 Identifier
        (Decode.field "what" Type.decoder)
        (Decode.field "identifiable" Uuid.decoder)
        (Decode.field "name" Decode.string)
        (Decode.field "fragments" (Decode.list Fragment.decoder))


toValue : Identifier -> String
toValue i =
    i.fragments |> List.map Fragment.toValue |> String.concat


update : Int -> Fragment -> Identifier -> Identifier
update index fragment identifier =
    let
        fragments =
            identifier.fragments
                |> List.indexedMap Tuple.pair
                |> List.map
                    (\( i, f ) ->
                        if i == index then
                            fragment

                        else
                            f
                    )
    in
    { identifier | fragments = fragments }
