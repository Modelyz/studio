module REA.ResourceType exposing (ResourceType, compare)


type alias ResourceType =
    { name : String
    , type_ : Maybe String
    }


compare : ResourceType -> String
compare =
    .name
