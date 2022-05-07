module REA.Entity exposing (Entity(..), decoder, encode)

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


type Entity
    = Resource
    | Event
    | Agent
    | Commitment
    | Contract
    | Process


encode : Entity -> Value
encode =
    toString >> Json.Encode.string


decoder : Decoder Entity
decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (fromString
                >> Maybe.map Json.Decode.succeed
                >> Maybe.withDefault (Json.Decode.fail "Unknown entity")
            )


fromString : String -> Maybe Entity
fromString s =
    case s of
        "Process" ->
            Just Process

        "Resource" ->
            Just Resource

        "Event" ->
            Just Event

        "Agent" ->
            Just Agent

        "Commitment" ->
            Just Commitment

        "Contract" ->
            Just Contract

        _ ->
            Nothing


toString : Entity -> String
toString e =
    case e of
        Process ->
            "Process"

        Resource ->
            "Resource"

        Event ->
            "Event"

        Agent ->
            "Agent"

        Commitment ->
            "Commitment"

        Contract ->
            "Contract"
