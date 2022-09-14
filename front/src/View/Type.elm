module View.Type exposing (Type(..), toString)

import Element exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Type
    = Smallcard
    | Table


toString : Type -> String
toString t =
    case t of
        Smallcard ->
            "Smallcard"

        Table ->
            "Table"
