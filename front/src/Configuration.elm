module Configuration exposing (Configuration, compare, decoder, encode)

import Entity.Type as Type exposing (Type)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import View.Lang as Lang exposing (Lang)
import View.Zone as Zone exposing (Zone)


type
    Configuration
    -- What can be configured and sent as a message
    = UIZoneConfig Zone Lang Type String


compare : Configuration -> String
compare config =
    case config of
        UIZoneConfig zone lang type_ name ->
            "UIZoneConfig" ++ " " ++ Zone.compare zone ++ " " ++ Lang.compare lang ++ " " ++ Type.toString type_ ++ " " ++ name


encode : Configuration -> Encode.Value
encode c =
    case c of
        UIZoneConfig zone lang type_ name ->
            Encode.object
                [ ( "what", Encode.string "UIZoneConfig" )
                , ( "zone", Zone.encode zone )
                , ( "lang", Lang.encode lang )
                , ( "type", Type.encode type_ )
                , ( "name", Encode.string name )
                ]


decoder : Decoder Configuration
decoder =
    Decode.field "what" Decode.string
        |> Decode.andThen
            (\what ->
                case what of
                    "UIZoneConfig" ->
                        Decode.map4 UIZoneConfig
                            (Decode.field "zone" Zone.decoder)
                            (Decode.field "lang" Lang.decoder)
                            (Decode.field "type" Type.decoder)
                            (Decode.field "name" Decode.string)

                    _ ->
                        Decode.fail <| "Unknown Configuration: " ++ what
            )
