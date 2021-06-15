module Contract exposing (..)

import Agent exposing (..)

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

