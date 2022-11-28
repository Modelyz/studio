module Expression.Eval exposing (Config, dleval, exeval, veval)

import Dict exposing (Dict)
import Expression exposing (..)
import Expression.Binary as B
import Expression.DeepLink as DeepLink exposing (DeepLink(..))
import Expression.HardLink as HardLink exposing (HardLink(..))
import Expression.Observable exposing (Observable(..))
import Expression.Rational as Rational exposing (Rational)
import Expression.Unary as U
import Expression.ValueSelection as ValueSelection exposing (ValueSelection(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope exposing (Scope(..), toType)
import Scope.State exposing (containsScope)
import Shared
import Type exposing (Type)
import Typed.Type as TType
import Util exposing (chooseIfSingleton)
import Value.Value exposing (..)


type alias Config =
    { context : ( Type, Uuid ) }


veval : Shared.Model -> Config -> Dict String Value -> Value -> Result String Rational
veval s c allVals value =
    exeval s c allVals value.expr


exeval : Shared.Model -> Config -> Dict String Value -> Expression -> Result String Rational
exeval s c allVals expr =
    case expr of
        Leaf obs ->
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
                                |> Result.andThen (.expr >> exeval s c allVals)

                ObsLink deeplink ->
                    dleval s deeplink [ Tuple.second c.context ]
                        |> chooseIfSingleton
                        |> Maybe.map (veval s c s.state.values)
                        |> Maybe.withDefault (Err "No result")

        Unary op e ->
            Result.map (U.eval op) (exeval s c allVals e)

        Binary op e f ->
            -- the error is displayed only for the 1st eval even if both fail
            B.eval op (exeval s c allVals e) (exeval s c allVals f)


step : Shared.Model -> HardLink -> Uuid -> List Uuid
step s hl uuid =
    {- For each hardlink encountered in the deeplink,
       return the list of entity uuids corresponding to the target of the hardlink
    -}
    case hl of
        CommitmentLink HardLink.CommitmentReceiver ->
            Dict.get (Uuid.toString uuid) s.state.commitments
                |> Maybe.map
                    (\ct ->
                        s.state.agents
                            |> Dict.filter (\_ v -> v.uuid == ct.receiver)
                            |> Dict.values
                            |> List.map .uuid
                    )
                |> Maybe.withDefault []

        AgentLink HardLink.AgentGroup ->
            s.state.grouped
                |> Dict.filter (\_ g -> g.groupable == uuid)
                |> Dict.values
                |> List.map .group

        _ ->
            []


dleval : Shared.Model -> DeepLink -> List Uuid -> List Value
dleval s deeplink currentlist =
    {- Recursive function that recompute each new list of uuid at each level of the deeplink,
       while transmitting the resulting uuid list to the endpoint, so that the final list of value can be computed
    -}
    case deeplink of
        Null ->
            -- end immediately
            []

        Link hardlink nextDeeplink ->
            -- run the eval one step further
            dleval s nextDeeplink (List.map (\uuid -> step s hardlink uuid) currentlist |> List.concat)

        EndPoint scope name ->
            -- use the transmitted list of uuid and find the corresponding values
            -- TODO not only GROUP!!
            let
                groups =
                    currentlist |> List.filter (\u -> toType scope |> Maybe.map (\t -> containsScope s.state.types (IsItem t u) scope) |> Maybe.withDefault False)
            in
            List.map (\g -> s.state.values |> Dict.filter (\_ v -> v.for == g) |> Dict.values) groups |> List.concat
