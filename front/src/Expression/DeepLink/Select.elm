module Expression.DeepLink.Select exposing (Model, Msg(..), init, update, view)

import Dict
import Element exposing (..)
import Element.Border as Border
import Expression.DeepLink as DL exposing (DeepLink(..))
import Expression.HardLink as HL exposing (HardLink)
import Scope.Scope exposing (Scope)
import Scope.State exposing (containsScope)
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
    | Choose DeepLink


init : Shared.Model -> Scope -> Model
init s scope =
    -- scope is the scope of the Value being added
    { scope = scope
    , deeplink = Null
    , stackNum = 0
    , targetPath = []
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
                button.primary (Choose model.deeplink) "Choose"

              else
                button.disabled "Please select a Value Type" "Choose"
            ]
        ]
    <|
        [ {- TODO turn into a chain of smallcards -} text <| DL.toDisplay model.deeplink
        , h2 "Choose between:"
        , wrappedRow [ spacing 20 ] <|
            case model.deeplink of
                {- TODO si on est sur null, on a notre contexte qui est le
                   scope choisi pour la value en cours de création.  Dans ce cas
                   on affiche les all du commitment (on extrait le type du scope).
                   Le contexte est donc un scope. Quand on choisit un hardlink, on
                   modifie le scope contexte en conséquence (sauf que là on n'a
                   pas de uuid donc le scope est un HasType). Si dans la
                   configuration du commitment pour le lien receiver on a un
                   scope, alors on remplace le scope en contexte par celui là.
                   Puis on affiche les all du nouveau contexte. On affiche aussi
                   les values correspondant au nouveau scope en contexte.

                      si on est sur Link, on vient de choisir un hardlink, on a un
                      nouveau scope qui est hastype (si pas de config sur le
                      hardlink) ou le scope du hardlink.

                       si on est sur EndPoint, alors on a sélectionné une value,
                       avec laquelle on veut remplacer le null. Dans ce cas on
                       affiche le bouton Choose.
                -}
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
            [ h2 "Select the value you want to choose:"
            , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
                (s.state.valueTypes
                    |> Dict.values
                    |> List.filter (\vt -> containsScope s.state.types vt.scope model.scope)
                    |> List.map (\vt -> clickableCard (Terminate vt.scope vt.name) (text vt.name) none)
                    |> withDefaultContent (text "(No Value Types are defined for this scope. Choose another entity or create a value for this entity)")
                )
            ]
        ]
