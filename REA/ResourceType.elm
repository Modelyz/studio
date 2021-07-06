module REA.ResourceType exposing (ResourceType)


type ResourceType
    = ResourceType
        { name : String
        , rtype : Maybe ResourceType
        }
