module REA.Identifier.Portion exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode



{- A Portion is a part of a generated identifier

   It can be :

   - a free string (such as: Free "prefix")
   - a constrained string
   - a sequence number
   - an pre-format fetched from the server

   Example for an sale order number:

   { name = "Sale Order Sequence"
   , entity = CommitmentType
   , unique = True
   , mandatory = True
   , format =
    [ Free "Prefix" "SO-"
    , YYYY
    , MM
    , DoM
    , Sequence "Number" 4 1 12
    , Free "Suffix" "/CAT3"
    ]

   should lead to a Sale Order Number of: "SO-20220508-0012i/CAT3"

   For a single "firstname" field it would be:

   { name = "Firstname"
   , entity = AgentType
   , type_ = "Person"
   , unique = False
   , mandatory = False
   , format = [ Free "" "John]
   }

-}


type Portion
    = Free Name
    | Sequence Name Padding Step
    | Existing Name
    | YYYY
    | YY
    | MMMM
    | MM
    | DoW
    | DoM
    | Hour
    | Minute
    | Second
    | DateFrom String


type alias Padding =
    Int


type alias Step =
    Int


type alias Name =
    String


all : List String
all =
    [ "Free"
    , "Sequence"
    , "Existing"
    , "YYYY"
    , "YY"
    , "MMMM"
    , "MM"
    , "DoW"
    , "DoM"
    , "Hour"
    , "Minute"
    , "Second"
    , "DateFrom"
    ]


toString : Portion -> String
toString p =
    case p of
        Free str ->
            "Free " ++ str

        Sequence name padding step ->
            "Sequence: " ++ name ++ " (padding=" ++ String.fromInt padding ++ ", step=" ++ String.fromInt step

        Existing str ->
            "Existing: " ++ str

        YYYY ->
            "YYYY (ex: 2022)"

        YY ->
            "YY (ex: 22)"

        MMMM ->
            "MMMM (ex: December)"

        MM ->
            "MM (ex: 12)"

        DoW ->
            "Day of week (ex: Sunday)"

        DoM ->
            "Day of month (ex: 23)"

        Hour ->
            "Hour"

        Minute ->
            "Minute"

        Second ->
            "Second"

        DateFrom from ->
            "Date from: " ++ from


encode : Portion -> Encode.Value
encode =
    \p ->
        case p of
            Free name ->
                Encode.object
                    [ ( "type", Encode.string "Free" )
                    , ( "name", Encode.string name )
                    ]

            Sequence name padding step ->
                Encode.object
                    [ ( "type", Encode.string "Sequence" )
                    , ( "name", Encode.string name )
                    , ( "padding", Encode.int padding )
                    , ( "step", Encode.int step )
                    ]

            Existing name ->
                Encode.object
                    [ ( "type", Encode.string "Existing" )
                    , ( "name", Encode.string name )
                    ]

            YYYY ->
                Encode.object
                    [ ( "type", Encode.string "YYYY" )
                    ]

            YY ->
                Encode.object
                    [ ( "type", Encode.string "YY" )
                    ]

            MMMM ->
                Encode.object
                    [ ( "type", Encode.string "MMMM" )
                    ]

            MM ->
                Encode.object
                    [ ( "type", Encode.string "MM" )
                    ]

            DoW ->
                Encode.object
                    [ ( "type", Encode.string "DoW" )
                    ]

            DoM ->
                Encode.object
                    [ ( "type", Encode.string "DoM" )
                    ]

            Hour ->
                Encode.object
                    [ ( "type", Encode.string "Hour" )
                    ]

            Minute ->
                Encode.object
                    [ ( "type", Encode.string "Minute" )
                    ]

            Second ->
                Encode.object
                    [ ( "type", Encode.string "Second" )
                    ]

            DateFrom name ->
                Encode.object
                    [ ( "type", Encode.string "DateFrom" )
                    , ( "name", Encode.string name )
                    ]


decoder : Decode.Decoder Portion
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Free" ->
                        Decode.map Free (Decode.field "name" Decode.string)

                    "Sequence" ->
                        Decode.map3 Sequence
                            (Decode.field "name" Decode.string)
                            (Decode.field "padding" Decode.int)
                            (Decode.field "step" Decode.int)

                    "Existing" ->
                        Decode.map Free (Decode.field "name" Decode.string)

                    "YYYY" ->
                        Decode.succeed YYYY

                    "YY" ->
                        Decode.succeed YY

                    "MMMM" ->
                        Decode.succeed MMMM

                    "MM" ->
                        Decode.succeed MM

                    "DoW" ->
                        Decode.succeed DoW

                    "DoM" ->
                        Decode.succeed DoM

                    "Hour" ->
                        Decode.succeed Hour

                    "Minute" ->
                        Decode.succeed Minute

                    "Second" ->
                        Decode.succeed Second

                    "DateFrom" ->
                        Decode.map DateFrom (Decode.field "name" Decode.string)

                    _ ->
                        Decode.fail t
            )
