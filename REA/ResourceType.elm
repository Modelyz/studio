module ResourceType exposing (..)

type ResourceType =
    ResourceType
        { name: String
        , rtype: Maybe ResourceType
        }
