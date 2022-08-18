module Ident.Identifier exposing (..)

import Dict exposing (Dict)
import Group.Group as Group exposing (Group)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Ident.Fragment as Fragment exposing (Fragment)
import Item.Item as Item exposing (Item, OnlyItem)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Typed.Typed exposing (Typed)


type alias Identifier =
    -- This is the value of an identifier
    { what : Type
    , identifiable : Uuid
    , name : String
    , fragments : List Fragment
    }


select : String -> Dict String Identifier -> Maybe Identifier
select name =
    Dict.filter (\_ i -> i.name == name) >> Dict.values >> List.head


fromUuid : Type -> Uuid -> Dict String Identifier -> Dict String Identifier
fromUuid what uuid =
    -- TODO rename fromUuid? Change args?
    Dict.filter (\_ i -> what == i.what && uuid == i.identifiable)


toDict : Dict String Identifier -> Dict String String
toDict ids =
    let
        agg : comparable -> Identifier -> Dict String String -> Dict String String
        agg _ i d =
            Dict.insert i.name (toValue i) d
    in
    Dict.foldl agg Dict.empty ids


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


match : String -> Identifier -> Bool
match string identifier =
    -- True if the string is contained in any fragment of the identifier
    Fragment.matchAny string identifier.fragments
