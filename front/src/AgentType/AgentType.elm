module AgentType.AgentType exposing (AgentType, compare, decoder, encode)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias AgentType =
    { what : Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , identifiers : Dict String Identifier -- TODO replace Dict String String with Dict String Identifier
    , groups : Dict String Group
    , display : Dict String String
    }


encode : AgentType -> Encode.Value
encode at =
    Encode.object <|
        -- we don't encode the identifiers, they are only for display
        [ ( "what", Type.encode at.what )
        , ( "uuid", Uuid.encode at.uuid )
        , ( "parent", Maybe.map Uuid.encode at.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder AgentType
decoder =
    Decode.map6 AgentType
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)


compare : AgentType -> String
compare =
    toString


toString : AgentType -> String
toString =
    .uuid >> Uuid.toString
