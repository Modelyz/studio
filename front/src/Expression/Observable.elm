module Expression.Observable exposing (Observable(..), allObs, decoder, encode, toString)

import Expression.DeepLink as DeepLink exposing (DeepLink(..))
import Expression.ValueSelection as VS exposing (ValueSelection(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type
    Observable
    -- a single number with a name and a value
    = Constant String -- with unit ??
    | ObsNumber { name : String, input : String }
      -- the value maybe existing for entity of gived type and uuid
    | ObsValue ValueSelection
    | ObsLink DeepLink


allObs : List Observable
allObs =
    [ constant "", number "" "", ObsValue UndefinedValue, ObsLink Null ]


toString : Observable -> String
toString obs =
    case obs of
        Constant _ ->
            "Constant"

        ObsNumber _ ->
            "Free Number"

        ObsValue _ ->
            "Other Value"

        ObsLink _ ->
            "Deep link"


number : String -> String -> Observable
number name input =
    ObsNumber { name = name, input = input }


constant : String -> Observable
constant =
    Constant


encode : Observable -> Encode.Value
encode obs =
    case obs of
        Constant n ->
            Encode.object [ ( "type", Encode.string "Constant" ), ( "value", Encode.string n ) ]

        ObsNumber n ->
            Encode.object
                [ ( "type", Encode.string "ObsNumber" )
                , ( "name", Encode.string n.name )
                , ( "input", Encode.string n.input )
                ]

        ObsValue vs ->
            Encode.object [ ( "type", Encode.string "ObsValue" ), ( "value", VS.encode vs ) ]

        ObsLink deeplink ->
            Encode.object [ ( "type", Encode.string "DeepLink" ), ( "value", DeepLink.encode deeplink ) ]


decoder : Decoder Observable
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Constant" ->
                        Decode.map Constant (Decode.field "value" Decode.string)

                    "ObsNumber" ->
                        Decode.map2 (\n i -> ObsNumber { name = n, input = i })
                            (Decode.field "name" Decode.string)
                            (Decode.field "input" Decode.string)

                    "ObsValue" ->
                        Decode.field "value" (Decode.map ObsValue VS.decoder)

                    "DeepLink" ->
                        Decode.field "value" (Decode.map ObsLink DeepLink.decoder)

                    _ ->
                        Decode.fail "Unknown Observable type"
            )
