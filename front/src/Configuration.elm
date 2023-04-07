module Configuration exposing (Configuration(..), compare, decoder, encode, getMostSpecific)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid exposing (Uuid)
import Scope as Scope exposing (Scope)
import Scope.State exposing (getUpperList)
import Type exposing (Type)
import Zone.Fragment as Fragment exposing (Fragment)
import Zone.Zone as Zone exposing (Zone)


type
    Configuration
    -- the list of identifier types to display on each zone
    = ZoneDisplay Zone (List Fragment) Scope


getConfig : Dict String Configuration -> Scope -> Maybe Configuration
getConfig configs scope =
    -- find the first config that correspond exactly to the scope
    List.head <| Dict.values <| Dict.filter (\_ (ZoneDisplay _ _ s) -> s == scope) configs


getMostSpecific : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String Configuration -> Zone -> Scope -> Maybe Configuration
getMostSpecific types configs _ scope =
    -- returns the most specific config for a given zone and scope
    findFirst configs (List.reverse <| getUpperList types scope [])


findFirst : Dict String Configuration -> List Scope -> Maybe Configuration
findFirst configs scopes =
    -- from a list of upperScopes, find the first one for which a config exists
    scopes
        |> List.head
        |> Maybe.map
            (\firstScope ->
                case getConfig configs firstScope of
                    Nothing ->
                        List.tail scopes
                            |> Maybe.andThen (findFirst configs)

                    Just config ->
                        Just config
            )
        |> Maybe.withDefault Nothing


compare : Configuration -> String
compare config =
    case config of
        ZoneDisplay zone _ scope ->
            "ZoneDisplay" ++ "/" ++ Zone.compare zone ++ "/" ++ Scope.compare scope


encode : Configuration -> Encode.Value
encode c =
    case c of
        ZoneDisplay zone names scope ->
            Encode.object
                [ ( "what", Encode.string "ZoneDisplay" )
                , ( "zone", Zone.encode zone )
                , ( "fragments", Encode.list Fragment.encode names )
                , ( "scope", Scope.encode scope )
                ]


decoder : Decoder Configuration
decoder =
    Decode.field "what" Decode.string
        |> Decode.andThen
            (\what ->
                case what of
                    "ZoneDisplay" ->
                        Decode.map3 ZoneDisplay
                            (Decode.field "zone" Zone.decoder)
                            (Decode.field "fragments" (Decode.list Fragment.decoder))
                            (Decode.field "scope" Scope.decoder)

                    _ ->
                        Decode.fail <| "Unknown Configuration: " ++ what
            )
