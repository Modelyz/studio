module REA.Type exposing (Type, decode, encode)

import DictSet as Set exposing (DictSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import REA.AgentType as AT exposing (AgentType)
import REA.CommitmentType as CMT exposing (CommitmentType)
import REA.ContractType as CNT exposing (ContractType)
import REA.Entity exposing (Entity(..))
import REA.EventType as ET exposing (EventType)
import REA.ProcessType as PT exposing (ProcessType)
import REA.ResourceType as RT exposing (ResourceType)


type alias Type =
    { name : String
    , parent : Maybe String
    }


encode : Type -> Encode.Value
encode t =
    Encode.object
        [ ( "name", Encode.string t.name )
        , ( "parent", Maybe.map Encode.string t.parent |> Maybe.withDefault Encode.null )
        ]


decode : Decoder Type
decode =
    Decode.map2
        Type
        (Decode.field "name" Decode.string)
        (Decode.field "parent" <| Decode.maybe Decode.string)
