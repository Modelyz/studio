module Configuration exposing (Configuration(..), compare, decoder, encode, getMostSpecific, onlyMenu, onlyZone)

import Configuration.Zone as Zone exposing (Zone)
import Configuration.Zone.Fragment as Fragment exposing (Fragment)
import Dict exposing (Dict)
import Hierarchy.Type as HType
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Scope exposing (Scope)
import Scope.State exposing (getUpperList)
import Type exposing (Type(..))


type
    Configuration
    -- the list of identifier types to display on each zone
    = ZoneDisplay { zone : Zone, fragments : List Fragment, scope : Scope }
    | MenuDisplay { what : HType.Type, uuid : Uuid, isMenu : Bool }


onlyMenu : Dict String Configuration -> Dict String Configuration
onlyMenu confs =
    confs
        |> Dict.filter
            (\_ v ->
                case v of
                    MenuDisplay _ ->
                        True

                    _ ->
                        False
            )


onlyZone : Maybe Zone -> Dict String Configuration -> Dict String Configuration
onlyZone mzone confs =
    confs
        |> Dict.filter
            (\_ v ->
                mzone
                    |> Maybe.map
                        (\requestedZone ->
                            case v of
                                ZoneDisplay display ->
                                    requestedZone == display.zone

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
                        ZoneDisplay display ->
                            display.zone == requestedZone && display.scope == scope

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
        ZoneDisplay display ->
            "ZoneDisplay" ++ "/" ++ Zone.compare display.zone ++ "/" ++ Scope.compare display.scope

        MenuDisplay display ->
            "MenuDisplay" ++ "/" ++ HType.compare display.what ++ "/" ++ Uuid.toString display.uuid


encode : Configuration -> Encode.Value
encode c =
    case c of
        ZoneDisplay display ->
            Encode.object
                [ ( "type", Encode.string "ZoneDisplay" )
                , ( "zone", Zone.encode display.zone )
                , ( "fragments", Encode.list Fragment.encode display.fragments )
                , ( "scope", Scope.encode display.scope )
                ]

        MenuDisplay display ->
            Encode.object
                [ ( "type", Encode.string "MenuDisplay" )
                , ( "what", HType.encode display.what )
                , ( "uuid", Encode.string (Uuid.toString display.uuid) )
                , ( "isMenu", Encode.bool display.isMenu )
                ]


decoder : Decoder Configuration
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "ZoneDisplay" ->
                        Decode.map3 (\z fs s -> ZoneDisplay { zone = z, fragments = fs, scope = s })
                            (Decode.field "zone" Zone.decoder)
                            (Decode.field "fragments" (Decode.list Fragment.decoder))
                            (Decode.field "scope" Scope.decoder)

                    "MenuDisplay" ->
                        Decode.map3 (\what uuid isMenu -> MenuDisplay { what = what, uuid = uuid, isMenu = isMenu })
                            (Decode.field "what" HType.decoder)
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "isMenu" Decode.bool)

                    _ ->
                        Decode.fail <| "Unknown Configuration type: " ++ type_
            )
