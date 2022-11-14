module Value.DeepLink.Select exposing (Model, Msg(..), init, update, view)

import Dict
import Element exposing (..)
import Element.Border as Border
import Hierarchy.Type as HType
import Scope.Scope as Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Shared
import State exposing (allHfromScope, allTfromScope)
import Type exposing (Type)
import Value.DeepLink as DeepLink exposing (DeepLink(..))
import Value.HardLink as Hardlink exposing (HardLink)
import Value.Value as Value exposing (Value)
import Value.ValueType exposing (ValueType)
import View exposing (..)
import View.Smallcard exposing (clickableCard, viewHalfCard)
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
            ( { model | deeplink = DeepLink.terminate scope name model.deeplink }, Cmd.none )

        AddedHardlink hardLink ->
            ( { model | deeplink = DeepLink.addTail hardLink model.deeplink }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Shared.Model -> Model -> Element Msg
view s model =
    floatingContainer s
        (Just Cancel)
        "Building a link to another Value"
        [ wrappedRow [ width fill, spacing 20 ]
            [ button.secondary Cancel "Cancel"
            , if DeepLink.isComplete model.deeplink then
                button.primary (Choose model.deeplink) "Choose"

              else
                button.disabled "Please select a Value Type" "Choose"
            ]
        ]
    <|
        [ {- TODO turn into a chain of smallcards -} text <| DeepLink.toDisplay model.deeplink
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
                    List.map (\l -> button.primary (AddedHardlink l) (Hardlink.toString l)) (DeepLink.toChoice model.scope)

                Link hl dl ->
                    -- TODO replace Nothing with Just scope of the restricted scope of the hardlink
                    List.map (\l -> button.primary (AddedHardlink l) (Hardlink.toString l)) (Hardlink.toChoice hl)

                EndPoint scope name ->
                    []
        ]
            ++ [ let
                    sc =
                        DeepLink.toScope model.scope model.deeplink

                    allT =
                        allTfromScope s.state sc

                    allH =
                        allHfromScope s.state sc
                 in
                 column [ spacing 20 ]
                    [ h2 "Select the value you want to choose:"
                    , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
                        (s.state.valueTypes
                            |> Dict.values
                            |> List.filter (\vt -> containsScope allT allH vt.scope sc)
                            |> List.map (\vt -> clickableCard (Terminate vt.scope vt.name) (text vt.name) none)
                            |> withDefaultContent (text "(No Value Types are defined for this scope. Choose another entity or create a value for this entity)")
                        )
                    ]
               ]
