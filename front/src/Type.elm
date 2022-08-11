module Type exposing (Type(..), all, allStrings, decoder, encode, fromType, isType, toPluralString, toString, toType)

import Hierarchy.Type as HType exposing (Type(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Typed.Type as TType exposing (Type(..))


type
    Type
    -- TODO see if it's still useful
    = TType TType.Type
    | HType HType.Type


all : List Type
all =
    [ HType ResourceType, HType EventType, HType AgentType, HType CommitmentType, HType ContractType, HType ProcessType, HType GroupType, TType Resource, TType Event, TType Agent, TType Commitment, TType Process, TType Group ]


allStrings : List String
allStrings =
    List.map toString all


isType : Type -> Bool
isType t =
    (String.slice 0 4 <| String.reverse <| toString t) == "epyT"


toType : Type -> Maybe Type
toType t =
    -- TODO rename to be more explicit
    if isType t then
        Just t

    else
        fromString <| (\ts -> ts ++ "Type") <| toString t


fromType : Type -> Maybe Type
fromType t =
    -- TODO rename to be more explicit
    if isType t then
        fromString <| String.slice 0 -4 <| toString t

    else
        Just t


toString : Type -> String
toString t =
    case t of
        TType tt ->
            TType.toString tt

        HType ht ->
            HType.toString ht


toPluralString : Type -> String
toPluralString t =
    -- TODO not i18n friendly
    case t of
        TType tt ->
            TType.toPluralString tt

        HType ht ->
            HType.toPluralString ht


fromString : String -> Maybe Type
fromString s =
    TType.fromString s
        |> Maybe.map TType
        |> (\t ->
                case t of
                    Just tt ->
                        Just tt

                    Nothing ->
                        HType.fromString s
                            |> Maybe.map HType
           )


encode : Type -> Encode.Value
encode =
    toString >> Encode.string


decoder : Decoder Type
decoder =
    Decode.string |> Decode.andThen (fromString >> Maybe.map Decode.succeed >> Maybe.withDefault (Decode.fail "Unknown Type"))
