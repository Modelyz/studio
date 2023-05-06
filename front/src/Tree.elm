module Tree exposing (TreeType(..), all, decoder, encode, isAscendentOf, isDescendentOf, isDirectChildOf, parentOf, parents, toString)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type
    TreeType
    -- non hierarchical group (or any tree-like structure)
    = Flat
      -- hierarchical and node-selectable
    | Node
      -- hierarchical and leaf-selectable
    | Leaf


isDirectChildOf : Dict String { a | parent : Maybe Uuid } -> Uuid -> Uuid -> Bool
isDirectChildOf items parent uuid =
    Dict.get (Uuid.toString uuid) items
        |> Maybe.andThen .parent
        |> Maybe.map ((==) parent)
        |> Maybe.withDefault False


isDescendentOf : Dict String { a | parent : Maybe Uuid } -> Uuid -> Uuid -> Bool
isDescendentOf items parent uuid =
    -- TODO merge with Type.ischildOf ?
    if uuid == parent then
        True

    else
        Dict.get (Uuid.toString uuid) items
            |> Maybe.andThen .parent
            |> Maybe.map
                (\puuid ->
                    if parent == puuid then
                        True

                    else
                        isDescendentOf items parent puuid
                )
            |> Maybe.withDefault False


isAscendentOf : Dict String { a | parent : Maybe Uuid } -> Uuid -> Uuid -> Bool
isAscendentOf entities uuid parent =
    isDescendentOf entities parent uuid


parentOf : Dict String { a | parent : Maybe Uuid } -> Uuid -> Maybe Uuid
parentOf entities uuid =
    Dict.get (Uuid.toString uuid) entities |> Maybe.andThen .parent


parents : Uuid -> Dict String { a | uuid : Uuid, parent : Maybe Uuid } -> List Uuid -> List Uuid
parents uuid items currents =
    Dict.get (Uuid.toString uuid) items
        |> Maybe.andThen .parent
        |> Maybe.map (\parent -> parents parent items (parent :: currents))
        |> Maybe.withDefault currents


all : List TreeType
all =
    [ Flat, Node, Leaf ]


toString : TreeType -> String
toString t =
    case t of
        Flat ->
            "Not hierarchical"

        Node ->
            "Hierarchical, an intermediate node can be selected"

        Leaf ->
            "Hierarchical, a leaf must be selected"


encode : TreeType -> Encode.Value
encode t =
    case t of
        Flat ->
            Encode.string "Flat"

        Node ->
            Encode.string "Node"

        Leaf ->
            Encode.string "Leaf"


decoder : Decoder TreeType
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
