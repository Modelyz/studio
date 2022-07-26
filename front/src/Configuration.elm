module Configuration exposing (Configuration(..), compare, decoder, encode, findFirst, only)

import DictSet as Set exposing (DictSet)
import Ident.Scope as Scope exposing (Scope)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import View.Type as ViewType exposing (Type)
import Zone.Fragment as Fragment exposing (Fragment)
import Zone.Zone as Zone exposing (Zone)


type
    Configuration
    -- the list of identifier types to display on each zone
    = ZoneConfig Zone (List Fragment) Scope


only : Zone -> DictSet String Configuration -> DictSet String Configuration
only zone cs =
    Set.filter
        (\c ->
            case c of
                ZoneConfig z _ _ ->
                    z == zone
        )
        cs


getConfig : DictSet String Configuration -> Scope -> Maybe Configuration
getConfig configs scope =
    List.head <| Set.toList <| Set.filter (\(ZoneConfig _ _ s) -> s == scope) configs


findFirst : DictSet String Configuration -> List Scope -> Maybe Configuration
findFirst configs scopes =
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
        ZoneConfig zone names scope ->
            "ZoneConfig" ++ " " ++ Zone.compare zone ++ " " ++ String.join " " (List.map Fragment.toString names)


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
