module Configuration exposing (Configuration(..), compare, decoder, encode, getMostSpecific)

import Dict exposing (Dict)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Scope.Scope as Scope exposing (Scope)
import Typed.Typed exposing (Typed)
import Zone.Fragment as Fragment exposing (Fragment)
import Zone.Zone as Zone exposing (Zone)


type
    Configuration
    -- the list of identifier types to display on each zone
    = ZoneConfig Zone (List Fragment) Scope


getConfig : Dict String Configuration -> Scope -> Maybe Configuration
getConfig configs scope =
    -- find the first config that correspond exactly to the scope
    List.head <| Dict.values <| Dict.filter (\_ (ZoneConfig _ _ s) -> s == scope) configs


getMostSpecific : Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Zone -> Scope -> Maybe Configuration
getMostSpecific allT allH configs zone scope =
    -- returns the most specific config for a given zone and scope
    findFirst configs (List.reverse <| Scope.getUpperList allT allH scope [])


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
        ZoneConfig zone _ scope ->
            "ZoneConfig" ++ "/" ++ Zone.compare zone ++ "/" ++ Scope.compare scope


encode : Configuration -> Encode.Value
encode c =
    case c of
        ZoneConfig zone names scope ->
            Encode.object
                [ ( "what", Encode.string "ZoneConfig" )
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
                    "ZoneConfig" ->
                        Decode.map3 ZoneConfig
                            (Decode.field "zone" Zone.decoder)
                            (Decode.field "fragments" (Decode.list Fragment.decoder))
                            (Decode.field "scope" Scope.decoder)

                    _ ->
                        Decode.fail <| "Unknown Configuration: " ++ what
            )
