module Tree exposing (Type(..), all, decoder, encode, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type
    Type
    -- non hierarchical group (or any tree-like structure)
    = Flat
      -- hierarchical and node-selectable
    | Node
      -- hierarchical and leaf-selectable
    | Leaf


all : List Type
all =
    [ Flat, Node, Leaf ]


encode : Type -> Encode.Value
encode t =
    case t of
        Flat ->
            Encode.string "Flat"

        Node ->
            Encode.string "Node"

        Leaf ->
            Encode.string "Leaf"


decoder : Decoder Type
decoder =
    Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Flat" ->
                        Decode.succeed Flat

                    "Node" ->
                        Decode.succeed Node

                    "Leaf" ->
                        Decode.succeed Leaf

                    _ ->
                        Decode.fail "Unknown Tree type"
            )


toString : Type -> String
toString t =
    case t of
        Flat ->
            "Not hierarchical"

        Node ->
            "Hierarchical, an intermediate node can be selected"

        Leaf ->
            "Hierarchical, a leaf must be selected"
