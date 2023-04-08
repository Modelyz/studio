module Configuration exposing (Configuration(..), compare, decoder, encode, getMostSpecific, onlyZone)

import Configuration.Zone as Zone exposing (Zone)
import Configuration.Zone.Fragment as Fragment exposing (Fragment)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Scope as Scope exposing (Scope)
import Scope.State exposing (getUpperList)
import Type exposing (Type)


type
    Configuration
    -- the list of identifier types to display on each zone
    = ZoneDisplay Zone (List Fragment) Scope
    | MenuDisplay Type Uuid Bool


onlyZone : Maybe Zone -> Dict String Configuration -> Dict String Configuration
onlyZone mzone confs =
    confs
        |> Dict.filter
            (\_ v ->
                mzone
                    |> Maybe.map
                        (\requestedZone ->
                            case v of
                                ZoneDisplay foundZone _ _ ->
                                    requestedZone == foundZone

                                _ ->
                                    False
                        )
                    -- we didn't specify a zone all zones are ok
                    |> Maybe.withDefault True
            )


getConfig : Zone -> Dict String Configuration -> Scope -> Maybe Configuration
getConfig requestedZone configs scope =
    -- find the first ZoneDisplay config that correspond exactly to the scope
    List.head <|
        Dict.values <|
            Dict.filter
                (\_ conf ->
                    case conf of
                        ZoneDisplay foundZone _ s ->
                            foundZone == requestedZone && s == scope

                        _ ->
                            False
                )
                configs


getMostSpecific : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String Configuration -> Zone -> Scope -> Maybe Configuration
getMostSpecific types configs zone scope =
    -- returns the most specific config for a given zone and scope
    findFirst zone configs (List.reverse <| getUpperList types scope [])


findFirst : Zone -> Dict String Configuration -> List Scope -> Maybe Configuration
findFirst zone configs scopes =
    -- from a list of upperScopes, find the first one for which a config exists
    scopes
        |> List.head
        |> Maybe.map
            (\firstScope ->
                case getConfig zone configs firstScope of
                    Nothing ->
                        List.tail scopes
                            |> Maybe.andThen (findFirst zone configs)

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
