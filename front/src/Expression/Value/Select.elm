module Expression.Value.Select exposing (Model, Msg(..), SelectedValue, init, update, view)

import Dict
import Element exposing (..)
import Element.Border as Border
import Expression.ValueSelection exposing (ValueSelection(..))
import Prng.Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import Scope.View exposing (selectScope)
import Shared
import Type exposing (Type)
import View exposing (..)
import View.Smallcard exposing (clickableCard, halfCard)
import View.Style exposing (..)


type SelectedValue
    = None
    | OnlyScope Scope
    | ScopeAndValue Scope String


type alias Model =
    { selection : SelectedValue
    , stackNum : Int
    , targetPath : List Int
    }


type Msg
    = InputScope Scope
    | InputValue Type Uuid String
    | Cancel
    | Choose ValueSelection Int (List Int)


init : Shared.Model -> Int -> List Int -> Model
init s stackNum targetPath =
    { selection = None
    , stackNum = stackNum
    , targetPath = targetPath
    }


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update s msg model =
    case msg of
        InputScope scope ->
            ( { model | selection = OnlyScope scope }, Cmd.none )

        InputValue _ _ name ->
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

        Choose _ _ _ ->
            ( { model | selection = None }, Cmd.none )


view : Shared.Model -> Model -> Element Msg
view s model =
    floatingContainer s
        (Just Cancel)
        "Selecting another Value"
        [ wrappedRow [ width fill, spacing 20 ]
            [ button.secondary Cancel "Cancel"
            , case model.selection of
                ScopeAndValue (IsItem type_ uuid) name ->
                    button.primary (Choose (SelectedValue type_ uuid name) model.stackNum model.targetPath) "Choose"

                _ ->
                    button.disabled "Please select a value" "Choose"
            ]
        ]
        (case model.selection of
            None ->
                [ selectScope s InputScope Scope.anything Scope.anything "Where do you want to select a value?" ]

            OnlyScope (IsItem type_ uuid) ->
                let
                    scope =
                        IsItem type_ uuid
                in
                [ selectScope s InputScope scope Scope.anything "Where do you want to select a value?"
                , selectValue s model scope (InputValue type_ uuid)
                ]

            OnlyScope scope ->
                [ selectScope s InputScope scope Scope.anything "Where do you want to select a value?" ]

            ScopeAndValue scope name ->
                [ selectScope s InputScope scope Scope.anything "Where do you want to select a value?"
                , halfCard (InputScope scope) (text name)
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
