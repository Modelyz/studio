module Value.Value exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (Posix)
import Type exposing (Type)
import Typed.Type as TType
import Value.Rational as R exposing (Rational(..))


type alias Value =
    { what : Type
    , for : Uuid
    , name : String
    , expr : Expression
    }


type Expression
    = Leaf Observable
    | Unary UOperator Expression
    | Binary BOperator Expression Expression


type BOperator
    = Add
    | Multiply
    | Or { name : String, desc : String, choice : Result String Bool }


type UOperator
    = Neg
    | Inv


type
    Observable
    -- a single number with a name and a value
    = ObsNumber { name : String, input : String, val : Result String Rational }
      -- the value maybe existing for entity of gived type and uuid
    | ObsValue ValueSelection
    | ObsLink DeepLink


type ValueSelection
    = SelectedValue Type Uuid String
    | UndefinedValue


type DeepLink
    = Link HardLink DeepLink
    | EndPoint


link : HardLink -> DeepLink -> DeepLink
link hl dl =
    Link hl dl


type
    HardLink
    -- automatically chosen depending on the context ie the selected scope
    = ResourceLink ResourceLink
    | EventLink EventLink
    | AgentLink AgentLink
    | CommitmentLink CommitmentLink
    | ContractLink ContractLink
    | ProcessLink ProcessLink
    | GroupLink GroupLink
    | ResourceTypeLink ResourceTypeLink
    | EventTypeLink EventTypeLink
    | AgentTypeLink AgentTypeLink
    | CommitmentTypeLink CommitmentTypeLink
    | ContractTypeLink ContractTypeLink
    | ProcessTypeLink ProcessTypeLink
    | GroupTypeLink GroupTypeLink


enlToString : HardLink -> String
enlToString x =
    case x of
        ResourceLink y ->
            rlToString y

        EventLink y ->
            elToString y

        AgentLink y ->
            alToString y

        CommitmentLink y ->
            cmlToString y

        ContractLink y ->
            cnlToString y

        ProcessLink y ->
            plToString y

        GroupLink y ->
            glToString y

        ResourceTypeLink y ->
            rtlToString y

        EventTypeLink y ->
            etlToString y

        AgentTypeLink y ->
            atlToString y

        CommitmentTypeLink y ->
            cmtlToString y

        ContractTypeLink y ->
            cntlToString y

        ProcessTypeLink y ->
            ptlToString y

        GroupTypeLink y ->
            gtlToString y


type ResourceLink
    = ResourceGroup
    | ResourceType


rlToString : ResourceLink -> String
rlToString x =
    case x of
        ResourceGroup ->
            "Resource Group"

        ResourceType ->
            "Resource Type"


allRL : List HardLink
allRL =
    [ ResourceLink ResourceGroup, ResourceLink ResourceType ]



--rlEval : Dict String Value -> ResourceLink -> Result String Value
--rlEval allVals link = case link of
--    ResourceGroup uuid l ->
--    ResourceType uuid l ->


type EventLink
    = EventProvider
    | EventReceiver
    | EventInflow
    | EventOutflow
    | EventGroup
    | EventType


allEL : List HardLink
allEL =
    [ EventLink EventProvider, EventLink EventReceiver, EventLink EventInflow, EventLink EventOutflow, EventLink EventGroup, EventLink EventType ]


elToString : EventLink -> String
elToString x =
    case x of
        EventProvider ->
            "Event Provider"

        EventReceiver ->
            "Event Receiver"

        EventInflow ->
            "Event Incoming resource"

        EventOutflow ->
            "Event Outgoing resource"

        EventGroup ->
            "Event Group"

        EventType ->
            "Event Type"


type AgentLink
    = AgentGroup
    | AgentType


allAL : List HardLink
allAL =
    [ AgentLink AgentGroup, AgentLink AgentType ]


alToString : AgentLink -> String
alToString x =
    case x of
        AgentGroup ->
            "Agent Group"

        AgentType ->
            "Agent Type"


type CommitmentLink
    = CommitmentProvider
    | CommitmentReceiver
    | CommitmentInflow
    | CommitmentOutflow
    | CommitmentGroup
    | CommitmentType


allCmL : List HardLink
allCmL =
    [ CommitmentLink CommitmentProvider, CommitmentLink CommitmentReceiver, CommitmentLink CommitmentInflow, CommitmentLink CommitmentOutflow, CommitmentLink CommitmentGroup, CommitmentLink CommitmentType ]


cmlToString : CommitmentLink -> String
cmlToString x =
    case x of
        CommitmentProvider ->
            "Commitment Provider"

        CommitmentReceiver ->
            "Commitment Receiver"

        CommitmentInflow ->
            "Commitment Incoming Resource"

        CommitmentOutflow ->
            "Commitment Outgoing Resource"

        CommitmentGroup ->
            "Commitment Group"

        CommitmentType ->
            "Commitment Type"


type ContractLink
    = ContractGroup
    | ContractType


allCnL : List HardLink
allCnL =
    [ ContractLink ContractGroup, ContractLink ContractType ]


cnlToString : ContractLink -> String
cnlToString x =
    case x of
        ContractGroup ->
            "Contract Group"

        ContractType ->
            "Contract Type"


type ProcessLink
    = ProcessGroup
    | ProcessType


allPL : List HardLink
allPL =
    [ ProcessLink ProcessGroup, ProcessLink ProcessType ]


plToString : ProcessLink -> String
plToString x =
    case x of
        ProcessGroup ->
            "Process Group"

        ProcessType ->
            "Process Type"


type GroupLink
    = GroupGroup
    | GroupType


allGL : List HardLink
allGL =
    [ GroupLink GroupGroup, GroupLink GroupType ]


glToString : GroupLink -> String
glToString x =
    case x of
        GroupGroup ->
            "Group Group"

        GroupType ->
            "Group Type"


type ResourceTypeLink
    = ResourceTypeGroup
    | ResourceTypeParent


allRTL : List HardLink
allRTL =
    [ ResourceTypeLink ResourceTypeGroup, ResourceTypeLink ResourceTypeParent ]


rtlToString : ResourceTypeLink -> String
rtlToString x =
    case x of
        ResourceTypeGroup ->
            "ResourceType Group"

        ResourceTypeParent ->
            "ResourceType Parent"


type EventTypeLink
    = EventTypeProvider
    | EventTypeReceiver
    | EventTypeInflow
    | EventTypeOutflow
    | EventTypeGroup
    | EventTypeType


allETL : List HardLink
allETL =
    [ EventTypeLink EventTypeProvider, EventTypeLink EventTypeReceiver, EventTypeLink EventTypeInflow, EventTypeLink EventTypeOutflow, EventTypeLink EventTypeGroup, EventTypeLink EventTypeType ]


etlToString : EventTypeLink -> String
etlToString x =
    case x of
        EventTypeProvider ->
            "Event Type Provider"

        EventTypeReceiver ->
            "Event Type Receiver"

        EventTypeInflow ->
            "Event Type Incoming Resource"

        EventTypeOutflow ->
            "Event Type Outgoing Resource"

        EventTypeGroup ->
            "Event Type Group"

        EventTypeType ->
            "Event Type Parent Type"


type AgentTypeLink
    = AgentTypeGroup
    | AgentTypeType


allATL : List HardLink
allATL =
    [ AgentTypeLink AgentTypeGroup, AgentTypeLink AgentTypeType ]


atlToString : AgentTypeLink -> String
atlToString x =
    case x of
        AgentTypeGroup ->
            "Agent Type Group"

        AgentTypeType ->
            "Agent Type Type"


type CommitmentTypeLink
    = CommitmentTypeProvider
    | CommitmentTypeReceiver
    | CommitmentTypeInflow
    | CommitmentTypeOutflow
    | CommitmentTypeGroup
    | CommitmentTypeType


allCmTL : List HardLink
allCmTL =
    [ CommitmentTypeLink CommitmentTypeProvider, CommitmentTypeLink CommitmentTypeReceiver, CommitmentTypeLink CommitmentTypeInflow, CommitmentTypeLink CommitmentTypeOutflow, CommitmentTypeLink CommitmentTypeGroup, CommitmentTypeLink CommitmentTypeType ]


cmtlToString : CommitmentTypeLink -> String
cmtlToString x =
    case x of
        CommitmentTypeProvider ->
            "Commitment Type Provider Agent"

        CommitmentTypeReceiver ->
            "Commitment Type Receiver Agent"

        CommitmentTypeInflow ->
            "Commitment Type Incoming Resource"

        CommitmentTypeOutflow ->
            "Commitment Type Outgoing Resource"

        CommitmentTypeGroup ->
            "Commitment Type Group"

        CommitmentTypeType ->
            "Commitment Type Parent Type"


type ContractTypeLink
    = ContractTypeGroup
    | ContractTypeType


allCnTL : List HardLink
allCnTL =
    [ ContractTypeLink ContractTypeGroup, ContractTypeLink ContractTypeType ]


cntlToString : ContractTypeLink -> String
cntlToString x =
    case x of
        ContractTypeGroup ->
            "Contract Type Group"

        ContractTypeType ->
            "Contract Type Type"


type ProcessTypeLink
    = ProcessTypeGroup
    | ProcessTypeType


allPTL : List HardLink
allPTL =
    [ ProcessTypeLink ProcessTypeGroup, ProcessTypeLink ProcessTypeType ]


ptlToString : ProcessTypeLink -> String
ptlToString x =
    case x of
        ProcessTypeGroup ->
            "Process Type Group"

        ProcessTypeType ->
            "Process Type Parent Type"


type GroupTypeLink
    = GroupTypeGroup
    | GroupTypeType


allGTL : List HardLink
allGTL =
    [ GroupTypeLink GroupTypeGroup, GroupTypeLink GroupTypeType ]


gtlToString : GroupTypeLink -> String
gtlToString x =
    case x of
        GroupTypeGroup ->
            "Group Type Group"

        GroupTypeType ->
            "Group Type Parent Type"


compare : Value -> String
compare v =
    Type.compare v.what ++ "/" ++ Uuid.toString v.for ++ "/" ++ v.name


fromUuid : Uuid -> Dict String Value -> Dict String Value
fromUuid uuid =
    Dict.filter (\_ v -> uuid == v.for)


encode : Value -> Encode.Value
encode v =
    Encode.object
        [ ( "what", Type.encode v.what )
        , ( "for", Uuid.encode v.for )
        , ( "name", Encode.string v.name )
        , ( "expr", eEncode v.expr )
        ]


decoder : Decoder Value
decoder =
    Decode.map4 Value
        (Decode.field "what" Type.decoder)
        (Decode.field "for" Uuid.decoder)
        (Decode.field "name" Decode.string)
        (Decode.field "expr" eDecoder)


updateExpr : List Int -> List Int -> Expression -> Expression -> Expression
updateExpr targetPath currentPath subExpr expr =
    -- we replace the expr at the given path
    case expr of
        Leaf obs ->
            if currentPath == targetPath then
                subExpr

            else
                expr

        Unary o e ->
            Unary o (updateExpr targetPath (1 :: currentPath) subExpr e)

        Binary o e1 e2 ->
            Binary o (updateExpr targetPath (2 :: currentPath) subExpr e1) (updateExpr targetPath (3 :: currentPath) subExpr e2)


uToString : UOperator -> String
uToString o =
    case o of
        Neg ->
            "Neg"

        Inv ->
            "Inv"


uToShortString : UOperator -> String
uToShortString o =
    case o of
        Neg ->
            " - "

        Inv ->
            "1 / "


bToString : BOperator -> String
bToString o =
    case o of
        Add ->
            "Add"

        Multiply ->
            "Multiply"

        Or _ ->
            "Or"


bToShortString : BOperator -> String
bToShortString o =
    case o of
        Add ->
            " + "

        Multiply ->
            " × "

        Or _ ->
            " or "


allObs : List Observable
allObs =
    [ number "" "", ObsValue UndefinedValue, ObsLink EndPoint ]


allUnary : List UOperator
allUnary =
    [ Neg, Inv ]


allBinary : List BOperator
allBinary =
    [ Add, Multiply, or "" "" Nothing ]


or : String -> String -> Maybe Bool -> BOperator
or n d c =
    Or { name = n, desc = d, choice = Result.fromMaybe "No choice made" c }


add : Expression -> Expression -> Expression
add e f =
    Binary Add e f


multiply : Expression -> Expression -> Expression
multiply e f =
    Binary Multiply e f


neg : Expression -> Expression
neg e =
    Unary Neg e


eval : Dict String Value -> Expression -> Result String Rational
eval allVals expr =
    case expr of
        Leaf obs ->
            oEval allVals obs

        Unary op e ->
            Result.map (uEval op) (eval allVals e)

        Binary op e f ->
            -- the error is displayed only for the 1st eval even if both fail
            bEval op (eval allVals e) (eval allVals f)


eEncode : Expression -> Encode.Value
eEncode e =
    case e of
        Leaf o ->
            Encode.object
                [ ( "type", Encode.string "Leaf" )
                , ( "obs", oEncode o )
                ]

        Unary operator expr ->
            Encode.object
                [ ( "type", Encode.string "Unary" )
                , ( "op", uEncode operator )
                , ( "expr", eEncode expr )
                ]

        Binary operator expr1 expr2 ->
            Encode.object
                [ ( "type", Encode.string "Binary" )
                , ( "op", bEncode operator )
                , ( "expr1", eEncode expr1 )
                , ( "expr2", eEncode expr2 )
                ]


eDecoder : Decoder Expression
eDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Leaf" ->
                        Decode.map Leaf (Decode.field "obs" oDecoder)

                    "Unary" ->
                        Decode.map2 Unary (Decode.field "op" uDecoder) (Decode.field "expr" eDecoder)

                    "Binary" ->
                        Decode.map3 Binary (Decode.field "op" bDecoder) (Decode.field "expr1" eDecoder) (Decode.field "expr2" eDecoder)

                    _ ->
                        Decode.fail "Unknown Expression"
            )


uEval : UOperator -> Rational -> Rational
uEval op n =
    case op of
        Neg ->
            R.neg n

        Inv ->
            R.inv n


bEval : BOperator -> Result String Rational -> Result String Rational -> Result String Rational
bEval operator res1 res2 =
    case operator of
        Add ->
            Result.map2 R.add res1 res2

        Multiply ->
            Result.map2 R.multiply res1 res2

        Or data ->
            -- one can choose either even if one fails
            data.choice
                |> Result.andThen
                    (\c ->
                        if c then
                            res1

                        else
                            res2
                    )


uEncode : UOperator -> Encode.Value
uEncode op =
    case op of
        Neg ->
            Encode.object [ ( "type", Encode.string "Neg" ) ]

        Inv ->
            Encode.object [ ( "type", Encode.string "Inv" ) ]


uDecoder : Decoder UOperator
uDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Neg" ->
                        Decode.succeed Neg

                    "Inv" ->
                        Decode.succeed Inv

                    _ ->
                        Decode.fail "Unknown Unary Operator"
            )


bEncode : BOperator -> Encode.Value
bEncode op =
    case op of
        Add ->
            Encode.object [ ( "type", Encode.string "+" ) ]

        Multiply ->
            Encode.object [ ( "type", Encode.string "*" ) ]

        Or data ->
            Encode.object
                [ ( "type", Encode.string "type" )
                , ( "name", Encode.string data.name )
                , ( "desc", Encode.string data.desc )
                , ( "choice", Result.map Encode.bool data.choice |> Result.withDefault Encode.null )
                ]


bDecoder : Decoder BOperator
bDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "+" ->
                        Decode.succeed Add

                    "*" ->
                        Decode.succeed Multiply

                    "Or" ->
                        Decode.map3 or (Decode.field "name" Decode.string) (Decode.field "desc" Decode.string) (Decode.field "choice" (Decode.nullable Decode.bool))

                    _ ->
                        Decode.fail "Unknown Unary Operator"
            )


vToString : ValueSelection -> String
vToString v =
    case v of
        SelectedValue _ _ _ ->
            "SelectedValue"

        UndefinedValue ->
            "UndefinedValue"


lToString : DeepLink -> String
lToString v =
    case v of
        Link hl dl ->
            "Link"

        EndPoint ->
            "EndPoint"


toString : Observable -> String
toString obs =
    case obs of
        ObsNumber _ ->
            "Free Number"

        ObsValue _ ->
            "Other Value"

        ObsLink _ ->
            "Deep link"


oEval : Dict String Value -> Observable -> Result String Rational
oEval allVals obs =
    case obs of
        ObsNumber n ->
            n.val

        ObsValue vs ->
            case vs of
                UndefinedValue ->
                    Err "Undefined"

                SelectedValue w f n ->
                    allVals
                        |> Dict.filter (\_ x -> x.what == w && x.for == f && x.name == n)
                        |> Dict.values
                        |> List.head
                        |> Result.fromMaybe "The value does not exist anymore"
                        |> Result.andThen (.expr >> eval allVals)

        ObsLink vs ->
            case vs of
                EndPoint ->
                    Err "Undefined"

                Link hl dl ->
                    Err "Undefined"


number : String -> String -> Observable
number name input =
    ObsNumber { name = name, input = input, val = Err "" }


oEncode : Observable -> Encode.Value
oEncode obs =
    case obs of
        ObsNumber n ->
            Encode.object
                [ ( "type", Encode.string "Number" )
                , ( "name", Encode.string n.name )
                , ( "input", Encode.string n.input )
                , ( "val", Result.map R.encode n.val |> Result.withDefault (Encode.string "") )
                ]

        ObsValue vs ->
            case vs of
                SelectedValue w f n ->
                    Encode.object
                        [ ( "type", Encode.string <| vToString vs )
                        , ( "what", Type.encode w )
                        , ( "for", Uuid.encode f )
                        , ( "name", Encode.string n )
                        ]

                UndefinedValue ->
                    Encode.object
                        [ ( "type", Encode.string <| vToString vs )
                        , ( "what", Encode.null )
                        , ( "for", Encode.null )
                        , ( "name", Encode.null )
                        ]

        ObsLink l ->
            case l of
                Link hl dl ->
                    Encode.object
                        [ ( "type", Encode.string <| lToString l )
                        ]

                EndPoint ->
                    Encode.object
                        [ ( "type", Encode.string <| lToString l )
                        , ( "what", Encode.null )
                        , ( "for", Encode.null )
                        , ( "name", Encode.null )
                        ]


oDecoder : Decoder Observable
oDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Number" ->
                        Decode.map3 (\n i v -> ObsNumber { name = n, input = i, val = v })
                            (Decode.field "name" Decode.string)
                            (Decode.field "input" Decode.string)
                            (Decode.field "val" R.decoder)

                    "SelectedValue" ->
                        Decode.map3 SelectedValue
                            (Decode.field "what" Type.decoder)
                            (Decode.field "for" Uuid.decoder)
                            (Decode.field "name" Decode.string)
                            |> Decode.andThen (\vs -> Decode.succeed (ObsValue vs))

                    "UndefinedValue" ->
                        Decode.succeed (ObsValue UndefinedValue)

                    "Link" ->
                        Decode.succeed (ObsValue UndefinedValue)

                    {- Decode.map2 (ObsLink << Link)
                       (Decode.field "hardLink" enlDecoder)
                       (Decode.field "deeplink" dlDecoder)
                    -}
                    "EndPoint" ->
                        Decode.succeed (ObsLink EndPoint)

                    _ ->
                        Decode.fail "Unknown Observable type"
            )
