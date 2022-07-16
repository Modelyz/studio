module Entity.Entity exposing (Entity(..), compare, decoder, encode, only, toPluralString, toString, toType, toUuid)

import Agent.Agent as Agent
import Commitment.Commitment as Commitment
import Contract.Contract as Contract exposing (Contract)
import DictSet as Set exposing (DictSet)
import EntityType.EntityType exposing (EntityType, toEntityString)
import EntityType.Type exposing (Type)
import Event.Event as Event
import Group.Group as Group
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process as Process
import Resource.Resource as Resource
import Time exposing (posixToMillis)


type Entity
    = Resource Resource.Resource
    | Event Event.Event
    | Agent Agent.Agent
    | Commitment Commitment.Commitment
    | Contract Contract.Contract
    | Process Process.Process
    | Group Group.Group


only : String -> DictSet String Entity -> DictSet String Entity
only t es =
    Set.filter (\e -> toString e == t) es


toType : Entity -> String
toType entity =
    case entity of
        Resource r ->
            r.type_

        Event e ->
            e.type_

        Agent a ->
            a.type_

        Commitment c ->
            c.type_

        Contract c ->
            c.type_

        Process p ->
            p.type_

        Group g ->
            g.type_


encode : Entity -> Value
encode entity =
    case entity of
        Resource r ->
            Encode.object
                [ ( "what", Encode.string "Resource" )
                , ( "value", Resource.encode r )
                ]

        Event e ->
            Encode.object
                [ ( "what", Encode.string "Event" )
                , ( "value", Event.encode e )
                ]

        Agent a ->
            Encode.object
                [ ( "what", Encode.string "Agent" )
                , ( "value", Agent.encode a )
                ]

        Commitment cm ->
            Encode.object
                [ ( "what", Encode.string "Commitment" )
                , ( "value", Commitment.encode cm )
                ]

        Contract cn ->
            Encode.object
                [ ( "what", Encode.string "Contract" )
                , ( "value", Contract.encode cn )
                ]

        Process p ->
            Encode.object
                [ ( "what", Encode.string "Process" )
                , ( "value", Process.encode p )
                ]

        Group g ->
            Encode.object
                [ ( "what", Encode.string "Group" )
                , ( "value", Group.encode g )
                ]


decoder : Decoder Entity
decoder =
    Decode.field "what" Decode.string
        |> Decode.andThen
            (\t ->
                Decode.field "value"
                    (case t of
                        "Resource" ->
                            Decode.map Resource Resource.decoder

                        "Event" ->
                            Decode.map Event Event.decoder

                        "Agent" ->
                            Decode.map Agent Agent.decoder

                        "Commitment" ->
                            Decode.map Commitment Commitment.decoder

                        "Contract" ->
                            Decode.map Contract Contract.decoder

                        "Process" ->
                            Decode.map Process Process.decoder

                        "Group" ->
                            Decode.map Group Group.decoder

                        _ ->
                            Decode.fail "Unknown entity"
                    )
            )


toUuid : Entity -> Uuid
toUuid e =
    case e of
        Process p ->
            p.uuid

        Resource r ->
            r.uuid

        Event ev ->
            ev.uuid

        Agent a ->
            a.uuid

        Commitment cm ->
            cm.uuid

        Contract cn ->
            cn.uuid

        Group g ->
            g.uuid


toString : Entity -> String
toString e =
    case e of
        Process p ->
            "Process"

        Resource r ->
            "Resource"

        Event ev ->
            "Event"

        Agent a ->
            "Agent"

        Commitment cm ->
            "Commitment"

        Contract cn ->
            "Contract"

        Group g ->
            "Group"


toPluralString : Entity -> String
toPluralString e =
    case e of
        Process _ ->
            "Processes"

        Resource _ ->
            "Resources"

        Event _ ->
            "Events"

        Agent _ ->
            "Agents"

        Commitment _ ->
            "Commitments"

        Contract _ ->
            "Contracts"

        Group _ ->
            "Groups"


compare : Entity -> String
compare =
    toUuid >> Uuid.toString
