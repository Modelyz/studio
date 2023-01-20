module Expression.DeepLink.Select exposing (Model, Msg(..), init, update, view)

import Dict
import Element exposing (..)
import Element.Border as Border
import Expression.DeepLink as DL exposing (DeepLink(..))
import Expression.DeepLink.View
import Expression.HardLink as HL exposing (HardLink)
import Scope exposing (Scope)
import Scope.State exposing (containsScope)
import Scope.View
import Shared
import View exposing (..)
import View.Smallcard exposing (clickableCard)
import View.Style exposing (..)


type alias Model =
    { scope : Scope
    , deeplink : DeepLink
    , stackNum : Int
    , targetPath : List Int
    }


type Msg
    = AddedHardlink HardLink
    | Cancel
    | Terminate Scope String
    | Choose DeepLink Int (List Int)


init : Shared.Model -> Scope -> Int -> List Int -> Model
init s scope stackNum targetPath =
    -- scope is the scope of the Value being added
    { scope = scope
    , deeplink = Null
    , stackNum = stackNum
    , targetPath = targetPath
    }


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update s msg model =
    case msg of
        Terminate scope name ->
            ( { model | deeplink = DL.terminate scope name model.deeplink }, Cmd.none )

        AddedHardlink hardLink ->
            ( { model
                | deeplink = DL.addTail hardLink model.deeplink
                , scope = HL.toScope hardLink
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Shared.Model -> Model -> Element Msg
view s model =
    floatingContainer s
        (Just Cancel)
        "Building a link to another Value"
        [ wrappedRow [ width fill, spacing 20 ]
            [ button.secondary Cancel "Cancel"
            , if DL.isComplete model.deeplink then
                button.primary (Choose model.deeplink model.stackNum model.targetPath) "Choose"

              else
                button.disabled "Please select a Value Type" "Choose"
            ]
        ]
    <|
        [ {- TODO turn into a chain of smallcards -} text <| Expression.DeepLink.View.toDisplay s model.deeplink
        , h2 "Choose between:"
        , wrappedRow [ spacing 20 ] <|
            case model.deeplink of
                Null ->
                    List.map (\l -> button.primary (AddedHardlink l) (HL.toString l)) (HL.chooseFromScope model.scope)

                Link hl _ ->
                    -- TODO replace Nothing with Just scope of the restricted scope of the hardlink
                    List.map (\l -> button.primary (AddedHardlink l) (HL.toString l)) (HL.chooseFromScope model.scope)

                EndPoint _ _ ->
                    []
        , let
            sc =
                DL.toScope model.scope model.deeplink
          in
          column [ spacing 20 ]
            [ h2 "Select the Value Type you want to choose:"
            , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
                (s.state.valueTypes
                    |> Dict.values
                    |> List.filter (\vt -> containsScope s.state.types vt.scope model.scope)
                    |> List.map (\vt -> clickableCard (Terminate vt.scope vt.name) (text vt.name) (text <| Scope.View.toDisplay s vt.scope))
                    |> withDefaultContent (p "(No Value Types are defined for this scope. Choose another entity or create a value type for this entity)")
                )
            ]
        ]
