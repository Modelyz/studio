module REA.Contract exposing (..)

import REA.Agent exposing (Agent)

type ContractType =
    ContractType
    { name: String
    , ctype: Maybe ContractType
    }


type Contract =
    Contract
    { name: String
    , ctype: ContractType
    , parties: List Agent
--    , clauses: 
--    , terms: 
    }

