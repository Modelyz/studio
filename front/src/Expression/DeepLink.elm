module Expression.DeepLink exposing (DeepLink(..), addTail, decoder, encode, isComplete, terminate, toDisplay, toScope)

import Expression.HardLink as HardLink exposing (HardLink)
import Hierarchy.Type as HType
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Scope.Scope as Scope exposing (Scope(..))
import Type
import Typed.Type as TType


type DeepLink
    = Null
    | Link HardLink DeepLink
      -- the endpoint corresponds to a ValueType
      -- the EndPoint scope is the restriction given by the last hardlink destination
    | EndPoint Scope String


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

        Link _ dl ->
            isComplete dl

        EndPoint _ _ ->
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

        EndPoint _ _ ->
            scope
