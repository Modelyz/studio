module Configuration exposing (Configuration(..), compare, decoder, encode, getMostSpecific, onlyZone)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Scope as Scope exposing (Scope)
import Scope.State exposing (getUpperList)
import Type exposing (Type)
import Zone.Fragment as Fragment exposing (Fragment)
import Zone.Zone as Zone exposing (Zone)


type
    Configuration
    -- the list of identifier types to display on each zone
    = ZoneDisplay Zone (List Fragment) Scope
    | MenuDisplay Type Uuid Bool


onlyZone : Dict String Configuration -> Dict String Configuration
onlyZone confs =
    confs
        |> Dict.filter
            (\_ v ->
                case v of
                    ZoneDisplay _ _ _ ->
                        True

                    _ ->
                        False
            )


getConfig : Dict String Configuration -> Scope -> Maybe Configuration
getConfig configs scope =
    -- find the first ZoneDisplay config that correspond exactly to the scope
    List.head <|
        Dict.values <|
            Dict.filter
                (\_ conf ->
                    case conf of
                        ZoneDisplay _ _ s ->
                            s == scope

                        _ ->
                            False
                )
                configs


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

        MenuDisplay type_ uuid _ ->
            "MenuDisplay" ++ "/" ++ Type.compare type_ ++ "/" ++ Uuid.toString uuid


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

        MenuDisplay type_ uuid visible ->
            Encode.object
                [ ( "what", Encode.string "MenuDisplay" )
                , ( "type", Type.encode type_ )
                , ( "uuid", Encode.string (Uuid.toString uuid) )
                , ( "visible", Encode.bool visible )
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
