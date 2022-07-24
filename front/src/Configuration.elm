module Configuration exposing (Configuration(..), compare, decoder, encode, only)

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
    = ZoneConfig Zone (List Fragment) (DictSet String Scope)



-- TODO remove
-- IdentifierType Nom / Agent(Nothing) Resource(Nothing)
-- IdentifierType Prénom / Agent(Personne)
-- IdentifierType NCODE / Agent(Adhérent)
-- Configuration SmallcardItemTitle / Agent(Nothing) Resource(Nothing) / Nom
-- Configuration SmallcardItemTitle / Agent(Personne) / Prénom Nom
-- Configuration SmallcardItemTitle / Agent(Adhérent) / NCODE Prénom Nom


only : Zone -> DictSet String Configuration -> DictSet String Configuration
only zone cs =
    Set.filter
        (\c ->
            case c of
                ZoneConfig z _ _ ->
                    z == zone
        )
        cs


compare : Configuration -> String
compare config =
    case config of
        ZoneConfig zone names scopes ->
            "ZoneConfig" ++ " " ++ Zone.compare zone ++ " " ++ String.join " " (List.map Fragment.toString names)


encode : Configuration -> Encode.Value
encode c =
    case c of
        ZoneConfig zone names scopes ->
            Encode.object
                [ ( "what", Encode.string "ZoneConfig" )
                , ( "zone", Zone.encode zone )
                , ( "fragments", Encode.list Fragment.encode names )
                , ( "scopes", Encode.list Scope.encode (Set.toList scopes) )
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
                            (Decode.field "scopes" (Decode.list Scope.decoder |> Decode.andThen (Set.fromList Scope.compare >> Decode.succeed)))

                    _ ->
                        Decode.fail <| "Unknown Configuration: " ++ what
            )
