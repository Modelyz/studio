module Agent exposing (..)


type AgentType =
    AgentType
        { name: String
        , atype: Maybe AgentType
         }


type Agent =
    Agent
        { name: String
        , atype: AgentType
         }



