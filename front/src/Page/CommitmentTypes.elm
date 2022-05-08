module Page.CommitmentTypes exposing (match, page, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.CommitmentType as CT exposing (CommitmentType)
import REA.ProcessType as PT exposing (ProcessType)
import REA.ProcessTypeCommitmentType exposing (ProcessTypeCommitmentType)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (..)


type alias Form =
    { name : String, ctype : Maybe String, processTypes : DictSet String String, warning : String }


empty : Form
empty =
    { name = "", ctype = Nothing, processTypes = Set.empty identity, warning = "" }


validate : Form -> Maybe ( CommitmentType, List ProcessType )
validate f =
    -- TODO replace with Either with the error on the Left
    if f.name == "" || Set.size f.processTypes == 0 then
        Nothing

    else
        Just
            ( { name = f.name, ctype = f.ctype }
            , f.processTypes |> Set.toList |> List.map (\pt -> { name = pt })
            )


type alias Model =
    { form : Form
    }


type alias Flags =
    ()


type Msg
    = Removed CommitmentType
    | Added ( CommitmentType, List ProcessType )
    | GotInput Form
    | Warning String
    | Link String
    | Unlink String


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init
        , update = update s
        , view = view s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.CommitmentTypes ->
            Just ()

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init _ =
    ( { form = empty }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Commitment Types"
    , attributes = []
    , element = viewContent s model
    }


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        GotInput form ->
            ( { model | form = form }, Effect.none )

        Added ( ctype, ptypes ) ->
            ( { model
                | form = empty
              }
            , Shared.dispatchMany s <|
                Event.CommitmentTypeAdded { commitmentType = CommitmentType ctype.name ctype.ctype }
                    :: List.map (\pt -> Event.LinkedCommitmentTypeToProcessType { ctype = ctype.name, ptype = pt.name }) ptypes
            )

        Removed ctype ->
            let
                form =
                    model.form
            in
            -- TODO UnlinkedCommitmentTypeToProcessType ?
            ( { model | form = { form | warning = "" } }
            , Shared.dispatch s <| Event.CommitmentTypeRemoved { commitmentType = ctype }
            )

        Warning w ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | warning = w } }, Effect.none )

        Link pt ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | processTypes = Set.insert pt model.form.processTypes } }, Effect.none )

        Unlink pt ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | processTypes = Set.remove pt model.form.processTypes } }, Effect.none )


viewContent : Shared.Model -> Model -> Element Msg
viewContent s model =
    let
        form =
            model.form
    in
    column [ width fill, alignTop, padding 20 ]
        [ column [ Border.shadow shadowStyle, padding 20, centerX, alignTop ]
            [ column
                [ spacing 20 ]
                [ h1 "Commitment Types"
                , if Set.size s.state.commitmentTypes > 0 then
                    p "Existing Commitment Types:"

                  else
                    p "There are no Commitment Types yet. Create your first one!"
                , wrappedRow
                    [ spacing 10 ]
                    (s.state.commitmentTypes
                        |> Set.toList
                        |> List.map (\pt -> viewSmallCard (Removed pt) pt.name "")
                    )
                , column
                    [ spacing 20 ]
                    [ text "Add a new Commitment Type:"
                    , row []
                        [ Input.text
                            [ Input.focusedOnLoad
                            , Maybe.map Added (validate model.form)
                                |> Maybe.withDefault (Warning "Incomplete form")
                                |> View.onEnter
                            ]
                            { onChange = \n -> GotInput { form | name = n }
                            , text = model.form.name
                            , placeholder =
                                Just <| Input.placeholder [] <| text "Name of the new Commitment Type"
                            , label = Input.labelLeft [] <| text "Name"
                            }
                        ]
                    , column
                        [ spacing 10 ]
                        [ p "This commitment type is usable from the following process types:"
                        , column [ spacing 10 ]
                            (s.state.processTypes
                                |> Set.toList
                                |> List.sortBy PT.compare
                                |> List.map
                                    (\pt ->
                                        row []
                                            [ row []
                                                [ Input.checkbox
                                                    []
                                                    { onChange =
                                                        \b ->
                                                            if b then
                                                                Link pt.name

                                                            else
                                                                Unlink pt.name
                                                    , icon = Input.defaultCheckbox
                                                    , checked = Set.member pt.name model.form.processTypes
                                                    , label = Input.labelRight [] <| text pt.name
                                                    }
                                                ]
                                            ]
                                    )
                            )
                        ]
                    , row [ spacing 20 ]
                        [ button.primary
                            { onPress =
                                Maybe.map Added (validate model.form)
                                    |> Maybe.withDefault (Warning "Incomplete form")
                                    |> Just
                            , label = text "Add"
                            }
                        , if model.form.warning /= "" then
                            paragraph [ Font.color color.text.warning ] [ text model.form.warning ]

                          else
                            none
                        ]
                    ]
                ]
            ]
        ]
