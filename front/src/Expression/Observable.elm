module Expression.Observable exposing (Observable(..), allObs, decoder, encode, toString)

import Expression.DeepLink as DeepLink exposing (DeepLink(..))
import Expression.Rational as Rational exposing (Rational)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid
import Type
import Value.ValueSelection as VS exposing (ValueSelection(..))


type
    Observable
    -- a single number with a name and a value
    = ObsNumber { name : String, input : String, val : Result String Rational }
      -- the value maybe existing for entity of gived type and uuid
    | ObsValue ValueSelection
    | ObsLink DeepLink


allObs : List Observable
allObs =
    [ number "" "", ObsValue UndefinedValue, ObsLink Null ]


toString : Observable -> String
toString obs =
    case obs of
        ObsNumber _ ->
            "Free Number"

        ObsValue _ ->
            "Other Value"

        ObsLink _ ->
            "Deep link"


number : String -> String -> Observable
number name input =
    ObsNumber { name = name, input = input, val = Err "" }


encode : Observable -> Encode.Value
encode obs =
    case obs of
        ObsNumber n ->
            Encode.object
                [ ( "type", Encode.string "Number" )
                , ( "name", Encode.string n.name )
                , ( "input", Encode.string n.input )
                , ( "val", Result.map Rational.encode n.val |> Result.withDefault (Encode.string "") )
                ]

        ObsValue vs ->
            case vs of
                SelectedValue w f n ->
                    Encode.object
                        [ ( "type", Encode.string <| VS.toString vs )
                        , ( "what", Type.encode w )
                        , ( "for", Uuid.encode f )
                        , ( "name", Encode.string n )
                        ]

                UndefinedValue ->
                    Encode.object
                        [ ( "type", Encode.string <| VS.toString vs )
                        , ( "what", Encode.null )
                        , ( "for", Encode.null )
                        , ( "name", Encode.null )
                        ]

        ObsLink deeplink ->
            Encode.object [ ( "type", Encode.string "DeepLink" ), ( "deeplink", DeepLink.encode deeplink ) ]


decoder : Decoder Observable
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Number" ->
                        Decode.map3 (\n i v -> ObsNumber { name = n, input = i, val = v })
                            (Decode.field "name" Decode.string)
                            (Decode.field "input" Decode.string)
                            (Decode.field "val" Rational.rdecoder)

                    "SelectedValue" ->
                        Decode.map ObsValue VS.decoder

                    "UndefinedValue" ->
                        Decode.succeed (ObsValue UndefinedValue)

                    "DeepLink" ->
                        Decode.field "deeplink" (Decode.map ObsLink DeepLink.decoder)

                    _ ->
                        Decode.fail "Unknown Observable type"
            )
