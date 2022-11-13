module Value.Select exposing (Model, Msg(..), init, selectValue, update, view)

import Dict
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import Scope.View exposing (selectScope)
import Shared
import Type exposing (Type)
import Value.ValueSelection exposing (ValueSelection(..))
import View exposing (..)
import View.Smallcard exposing (clickableCard, viewHalfCard)
import View.Style exposing (..)


type SelectedValue
    = None
    | OnlyScope Scope
    | ScopeAndValue Scope String


type alias Model =
    { selection : SelectedValue
    , onSelect : ValueSelection -> Msg
    , stackNum : Int
    , targetPath : List Int
    }


type Msg
    = InputScope Scope
    | InputValue Type Uuid String
    | Cancel
    | Selected ValueSelection


init : Shared.Model -> Model
init s =
    { selection = None
    , onSelect = Selected
    , stackNum = 0
    , targetPath = []
    }


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update s msg model =
    case msg of
        InputScope scope ->
            ( { model | selection = OnlyScope scope }, Cmd.none )

        InputValue type_ uuid name ->
            ( { model
                | selection =
                    case model.selection of
                        OnlyScope scope ->
                            ScopeAndValue scope name

                        _ ->
                            None
              }
            , Cmd.none
            )

        Cancel ->
            ( { model | selection = None }, Cmd.none )

        Selected vs ->
            case vs of
                UndefinedValue ->
                    ( model, Cmd.none )

                SelectedValue _ _ _ ->
                    ( model, Cmd.none )


view : Shared.Model -> Model -> Element Msg
view s model =
    floatingContainer s
        (Just Cancel)
        "Selecting another Value"
        [ wrappedRow [ width fill, spacing 20 ]
            [ button.secondary Cancel "Cancel"
            , case model.selection of
                ScopeAndValue (IsItem type_ uuid) name ->
                    button.primary (model.onSelect <| SelectedValue type_ uuid name) "Choose"

                _ ->
                    button.disabled "Please select a value" "Choose"
            ]
        ]
        (case model.selection of
            None ->
                [ selectScope s InputScope Empty ]

            OnlyScope (IsItem type_ uuid) ->
                let
                    scope =
                        IsItem type_ uuid
                in
                [ selectScope s InputScope scope
                , selectValue s model scope (InputValue type_ uuid)
                ]

            OnlyScope scope ->
                [ selectScope s InputScope scope ]

            ScopeAndValue scope name ->
                [ selectScope s InputScope scope
                , viewHalfCard (Just <| InputScope scope) (text name)
                ]
        )


selectValue : Shared.Model -> Model -> Scope -> (String -> Msg) -> Element Msg
selectValue s model scope onInput =
    case scope of
        IsItem t uuid ->
            column [ spacing 20 ]
                [ h2 "Select the value you want to choose:"
                , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
                    (s.state.values
                        |> Dict.values
                        |> List.filter (\v -> v.what == t && v.for == uuid)
                        |> List.map (\v -> clickableCard (onInput v.name) (text v.name) none)
                        |> withDefaultContent (text "(No values are defined for this entity. Choose another entity or create a value for this entity)")
                    )
                ]

        _ ->
            none
