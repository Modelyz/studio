module Value.DeepLink exposing (DeepLink(..), addTail, decoder, encode, isComplete, terminate, toChoice, toDisplay, toScope, toString)

import Dict
import Hierarchy.Type as HType
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Scope.Scope as Scope exposing (Scope(..))
import Type exposing (Type)
import Typed.Type as TType
import Value.HardLink as HardLink exposing (HardLink)


type DeepLink
    = Null
    | Link HardLink DeepLink
      -- the endpoint corresponds to a ValueType
      -- the EndPoint scope is the restriction given by the last hardlink destination
    | EndPoint Scope String


toString : DeepLink -> String
toString v =
    case v of
        Link hl dl ->
            "Link"

        EndPoint _ name ->
            -- FIXME
            "EndPoint : " ++ name

        Null ->
            -- FIXME
            "Null"


encode : DeepLink -> Encode.Value
encode deeplink =
    case deeplink of
        -- TODO we shouldn't be able to add an incomplete deeplink here
        Link hl dl ->
            -- FIXME
            Encode.object
                [ ( "type", Encode.string "Link" )
                , ( "hardlink", HardLink.encode hl )
                , ( "deeplink", encode dl )
                ]

        Null ->
            -- FIXME
            Encode.object
                [ ( "type", Encode.string "Null" )
                ]

        EndPoint scope name ->
            -- FIXME
            Encode.object
                [ ( "type", Encode.string "Endpoint" )
                , ( "scope", Scope.encode scope )
                , ( "name", Encode.string name )
                ]


decoder : Decoder DeepLink
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Link" ->
                        Decode.map2 Link
                            (Decode.field "hardlink" HardLink.decoder)
                            (Decode.field "deeplink" decoder)

                    "Null" ->
                        Decode.succeed Null

                    "Endpoint" ->
                        Decode.map2 EndPoint
                            (Decode.field "scope" Scope.decoder)
                            (Decode.field "name" Decode.string)

                    _ ->
                        Decode.fail "Unknown type of DeepLink"
            )


addTail : HardLink -> DeepLink -> DeepLink
addTail hl dl =
    case dl of
        Null ->
            Link hl dl

        Link hl2 dl2 ->
            Link hl2 (addTail hl dl2)

        EndPoint scope name ->
            Link hl (EndPoint scope name)


isComplete : DeepLink -> Bool
isComplete deeplink =
    case deeplink of
        Null ->
            False

        Link hl dl ->
            isComplete dl

        EndPoint scope name ->
            True


terminate : Scope -> String -> DeepLink -> DeepLink
terminate scope name dl =
    case dl of
        Null ->
            EndPoint scope name

        Link hl dl2 ->
            Link hl (terminate scope name dl2)

        EndPoint _ _ ->
            EndPoint scope name


toDisplay : DeepLink -> String
toDisplay deeplink =
    case deeplink of
        Null ->
            "Empty"

        Link hl dl ->
            HardLink.toString hl ++ " → " ++ toDisplay dl

        EndPoint scope name ->
            -- TODO display without uuid
            -- name ++ " (" Scope.View.toDisplay allT allG configs scope ++ ")"
            name ++ " (" ++ Scope.toString scope ++ ")"


toScope : Scope -> DeepLink -> Scope
toScope scope deeplink =
    -- TODO pass the restriction scope on the hardlink to narrow the choice of values
    case deeplink of
        Null ->
            scope

        Link hl dl ->
            toScope (HardLink.toScope hl) dl

        EndPoint vtscope name ->
            scope


toChoice : Scope -> List HardLink
toChoice scope =
    case scope of
        HasUserType t uuid ->
            case t of
                Type.TType TType.Resource ->
                    HardLink.allRL

                Type.TType TType.Event ->
                    HardLink.allEL

                Type.TType TType.Agent ->
                    HardLink.allAL

                Type.TType TType.Commitment ->
                    HardLink.allCmL

                Type.TType TType.Contract ->
                    HardLink.allCnL

                Type.TType TType.Process ->
                    HardLink.allPL

                Type.TType TType.Group ->
                    HardLink.allGL

                Type.HType HType.ResourceType ->
                    HardLink.allRL

                Type.HType HType.EventType ->
                    HardLink.allEL

                Type.HType HType.AgentType ->
                    HardLink.allAL

                Type.HType HType.CommitmentType ->
                    HardLink.allCmL

                Type.HType HType.ContractType ->
                    HardLink.allCnL

                Type.HType HType.ProcessType ->
                    HardLink.allPL

                Type.HType HType.GroupType ->
                    HardLink.allGL

        _ ->
            []
