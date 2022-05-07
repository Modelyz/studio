module REA.Entity exposing (Entity(..), decoder, encode)

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


type Entity
    = Resource
    | Event
    | Agent
    | Commitment
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
        "Resource" ->
            Just Resource

        "Event" ->
            Just Event

        "Agent" ->
            Just Agent

        "Commitment" ->
            Just Commitment

        "Process" ->
            Just Process

        _ ->
            Nothing


toString : Entity -> String
toString e =
    case e of
        Resource ->
            "Resource"

        Event ->
            "Event"

        Agent ->
            "Agent"

        Commitment ->
            "Commitment"

        Process ->
            "Process"
