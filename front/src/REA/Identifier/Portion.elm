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
    | Fixed String
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


all : List Portion
all =
    [ Free ""
    , Fixed ""
    , Sequence "" 4 1
    , Existing ""
    , YYYY
    , YY
    , MMMM
    , MM
    , DoW
    , DoM
    , Hour
    , Minute
    , Second
    , DateFrom ""
    ]


toString : Portion -> String
toString p =
    case p of
        Free name ->
            "Free"

        Fixed str ->
            "Fixed"

        Sequence name padding step ->
            "Sequence"

        Existing str ->
            "Existing"

        YYYY ->
            "YYYY"

        YY ->
            "YY"

        MMMM ->
            "MMMM"

        MM ->
            "MM"

        DoW ->
            "DoW"

        DoM ->
            "DoM"

        Hour ->
            "hh"

        Minute ->
            "mm"

        Second ->
            "ss"

        DateFrom from ->
            "Date from: " ++ from


toDesc : Portion -> String
toDesc p =
    case p of
        Free name ->
            "A free text that you will be able to enter when adding a new TODO. For instance a firstname of an Agent"

        Fixed str ->
            "A fixed text that you must configure now and that will be always the same. For instance it can be a prefix or a suffix"

        Sequence name padding step ->
            "A sequence number"

        Existing str ->
            "An existing identifier"

        YYYY ->
            "The year on 4 digits"

        YY ->
            "The year on 2 digits"

        MMMM ->
            "The month in long form. Ex: August"

        MM ->
            "The month on 2 digits."

        DoW ->
            "Day of week in long form (Ex: Saturday)"

        DoM ->
            "Day of month on 2 digits"

        Hour ->
            "Hour on 2 digits"

        Minute ->
            "Minute on 2 digits"

        Second ->
            "Second on 2 digits"

        DateFrom from ->
            "Date coming from another entity: " ++ from


encode : Portion -> Encode.Value
encode =
    \p ->
        case p of
            Free name ->
                Encode.object
                    [ ( "type", Encode.string "Free" )
                    , ( "name", Encode.string name )
                    ]

            Fixed string ->
                Encode.object
                    [ ( "type", Encode.string "Fixed" )
                    , ( "string", Encode.string string )
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

                    "Fixed" ->
                        Decode.map Free (Decode.field "string" Decode.string)

                    "Sequence" ->
                        Decode.map3 Sequence
                            (Decode.field "name" Decode.string)
                            (Decode.field "padding" Decode.int)
                            (Decode.field "step" Decode.int)

                    "Existing" ->
                        Decode.map Existing (Decode.field "name" Decode.string)

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
