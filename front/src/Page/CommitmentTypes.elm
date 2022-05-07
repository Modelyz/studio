module Page.CommitmentTypes exposing (match, page, view)

import DictSet as Set
import Effect exposing (Effect)
import Element exposing (..)
import Element.Input as Input exposing (button)
import Event
import Html.Attributes as Attr
import REA.CommitmentType as CT exposing (CommitmentType)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View, onEnter)


type alias Model =
    { inputCommitmentType : String
    , inputCommitmentTypeProcessTypes : Set.DictSet String String
    }


type alias Flags =
    { route : Route }


type Msg
    = InputCommitmentType String
    | LinkToProcessType String
    | UnlinkFromProcessType String
    | NewCommitmentType String
    | DeleteCommitmentType CommitmentType


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init s
        , update = update s
        , view = view s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.CommitmentTypes ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init _ _ =
    ( { inputCommitmentType = ""
      , inputCommitmentTypeProcessTypes = Set.empty identity
      }
    , Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputCommitmentType ctype ->
            ( { model | inputCommitmentType = ctype }, Effect.none )

        DeleteCommitmentType ctype ->
            ( model
            , Shared.dispatch s <| Event.CommitmentTypeRemoved { commitmentType = ctype }
            )

        LinkToProcessType pt ->
            ( { model | inputCommitmentTypeProcessTypes = Set.insert pt model.inputCommitmentTypeProcessTypes }, Effect.none )

        UnlinkFromProcessType pt ->
            ( { model | inputCommitmentTypeProcessTypes = Set.remove pt model.inputCommitmentTypeProcessTypes }, Effect.none )

        NewCommitmentType name ->
            ( { model
                | inputCommitmentType = ""
                , inputCommitmentTypeProcessTypes = Set.empty identity
              }
            , Shared.dispatch s <| Event.CommitmentTypeAdded { commitmentType = CT.new name }
            )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Commitment Types"
    , attributes = []
    , element = viewContent s model
    }


viewThumbnail : CommitmentType -> Element Msg
viewThumbnail ct =
    row []
        [ row
            [ htmlAttribute <| Attr.id ct.name ]
            [ button
                []
                { onPress = Just <| DeleteCommitmentType ct, label = text ct.name }
            ]
        ]


viewContent : Shared.Model -> Model -> Element Msg
viewContent s model =
    row
        []
        [ row
            []
            [ row []
                [ paragraph []
                    [ text "Commitment Types"
                    ]
                , paragraph [] [ text "What kind of events may be expected to happen in the future" ]
                ]
            ]
        , row
            []
            [ row
                []
                ((if Set.size s.state.commitmentTypes > 0 then
                    row [] [ text "Current types:" ]

                  else
                    column [] []
                 )
                    :: (s.state.commitmentTypes
                            |> Set.toList
                            |> List.map viewThumbnail
                       )
                )
            , row
                []
                [ row
                    []
                    [ text "Add a new Commitment type:" ]
                , row []
                    [ row
                        []
                        [ Input.text
                            [ onEnter <| NewCommitmentType model.inputCommitmentType ]
                            { onChange = InputCommitmentType
                            , text = model.inputCommitmentType
                            , placeholder =
                                if model.inputCommitmentType == "" then
                                    Just <| Input.placeholder [] <| text "Enter the name of a new commitment type"

                                else
                                    Nothing
                            , label = Input.labelLeft [] (text "Commitment Type")
                            }
                        ]
                    ]
                , row []
                    [ row
                        []
                        [ button []
                            { onPress = Just <| NewCommitmentType model.inputCommitmentType, label = text "Add" }
                        ]
                    ]
                ]
            ]
        ]
