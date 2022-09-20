module View.Type exposing (Type(..), toString)


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
