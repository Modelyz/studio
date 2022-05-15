module Page.AddIdentifier exposing (..)

import Browser.Navigation as Nav
import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.Entity as Entity exposing (Entity, toPluralString)
import REA.Identifier as I exposing (..)
import REA.Identifier.Portion as Portion exposing (Portion(..))
import Result exposing (andThen)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.Navbar as Navbar
import View.Radio as Radio


type Msg
    = InputName String
    | InputEntity Entity
    | InputUnique Bool
    | InputMandatory Bool
    | InputFormat (List Portion)
    | Warning String
    | PreviousPage Int
    | NextPage Int
    | Cancel
    | Added Identifier


type alias Flags =
    { route : Route
    }


type alias Model =
    { previous : Route
    , route : Route
    , name : String
    , entity : Maybe Entity
    , unique : Bool
    , mandatory : Bool
    , format : List Portion
    , warning : String
    , step : Step
    }


type Step
    = Name
    | Entity
    | Options
    | Format


steps : List Step
steps =
    [ Entity, Options, Format, Name ]


checkEmpty : String -> String -> Result String String
checkEmpty field err =
    if String.isEmpty field then
        Err err

    else
        Ok field


checkNothing : Maybe a -> String -> Result String a
checkNothing field err =
    case field of
        Nothing ->
            Err "You must select an Entity"

        Just x ->
            Ok x


validate : Model -> Result String Identifier
validate m =
    Result.map5
        Identifier
        (checkEmpty m.name "The name is Empty")
        (checkNothing m.entity "You must select an entity")
        (Ok m.unique)
        (Ok m.mandatory)
        (Ok m.format)


indexOf : a -> List a -> Maybe Int
indexOf x =
    -- 1st index is 1
    List.indexedMap Tuple.pair >> List.filter (\z -> x == Tuple.second z) >> List.head >> Maybe.map Tuple.first


getItem : Int -> List a -> Maybe a
getItem i =
    List.indexedMap Tuple.pair >> List.filter (\x -> i == Tuple.first x) >> List.head >> Maybe.map Tuple.second


previous : a -> List a -> Maybe a
previous x xs =
    indexOf x xs |> Maybe.andThen (\i -> getItem (i - 1) xs)


next : a -> List a -> Maybe a
next x xs =
    indexOf x xs |> Maybe.andThen (\i -> getItem (i + 1) xs)


isLast : a -> List a -> Bool
isLast x xs =
    indexOf x xs |> Maybe.map ((==) (List.length xs - 1)) |> Maybe.withDefault False


isFirst : a -> List a -> Bool
isFirst x xs =
    indexOf x xs |> Maybe.map ((==) 0) |> Maybe.withDefault False


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
    -- TODO give the entity to create through the flags? /add/identifier?step=2
    case route of
        Route.AddIdentifier ->
            Just { route = route }

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init f =
    ( { previous = Route.Identifiers
      , route = f.route
      , name = ""
      , entity = Nothing
      , unique = False
      , mandatory = False
      , format = []
      , warning = ""
      , step = Entity
      }
    , Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputName x ->
            ( { model | name = x }, Effect.none )

        InputEntity x ->
            ( { model | entity = Just x }, Effect.none )

        InputUnique x ->
            ( { model | unique = x }, Effect.none )

        InputMandatory x ->
            ( { model | mandatory = x }, Effect.none )

        InputFormat x ->
            ( { model | format = x }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Added i ->
            ( model
            , Effect.batch
                [ Shared.dispatch s <| Event.IdentifierAdded i
                , goTo s Route.Identifiers
                ]
            )

        PreviousPage step ->
            case previous model.step steps of
                Just x ->
                    ( { model
                        | step = x
                      }
                    , Effect.none
                    )

                Nothing ->
                    ( model, Effect.fromCmd <| Nav.pushUrl s.navkey <| Route.toString Route.Identifiers )

        NextPage step ->
            case next model.step steps of
                Just x ->
                    ( { model
                        | step = x
                      }
                    , Effect.none
                    )

                Nothing ->
                    ( model, Effect.fromCmd <| Nav.pushUrl s.navkey <| Route.toString Route.Identifiers )

        Cancel ->
            ( model, goTo s Route.Identifiers )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Event Types"
    , attributes = []
    , element =
        row
            [ width fill, height fill ]
            [ Navbar.view s model
            , viewContent s model
            ]
    }


viewContent : Shared.Model -> Model -> Element Msg
viewContent s model =
    let
        check =
            case validate model of
                Ok f ->
                    Added f

                Err err ->
                    Warning ("Error: " ++ err)

        nbsteps =
            List.length steps

        buttons =
            [ wrappedRow [ width fill, spacing 20 ]
                [ button.secondary
                    { onPress =
                        Just <|
                            if isFirst model.step steps then
                                Cancel

                            else
                                PreviousPage nbsteps
                    , label =
                        text "← Previous"
                    }
                , button.secondary
                    { onPress = Just <| Cancel
                    , label =
                        text "Cancel"
                    }
                , if isLast model.step steps then
                    button.primary { onPress = Just check, label = text "Validate and add this identifier" }

                  else
                    button.secondary { onPress = Just <| NextPage nbsteps, label = text "Next →" }
                , if model.warning /= "" then
                    paragraph [ Font.color color.text.warning ] [ text model.warning ]

                  else
                    none
                ]
            ]

        step =
            case model.step of
                Entity ->
                    row [ alignTop, width <| minimum 200 fill, Font.size size.text.h3 ]
                        [ Radio.view
                            { title = "Apply to Which Entity?"
                            , options = Entity.all |> List.map (\e -> ( e, toPluralString e ))
                            , selected = model.entity
                            , msg =
                                \e -> InputEntity e
                            }
                        ]

                Options ->
                    column [ alignTop, width <| minimum 200 fill, spacing 10 ]
                        [ h3 "Options:"
                        , row [ Font.size size.text.main ]
                            [ Input.checkbox
                                []
                                { onChange = InputUnique
                                , icon = Input.defaultCheckbox
                                , checked = model.unique
                                , label = Input.labelRight [] <| text "Each value is unique"
                                }
                            ]
                        , row [ Font.size size.text.main ]
                            [ Input.checkbox
                                []
                                { onChange = InputMandatory
                                , icon = Input.defaultCheckbox
                                , checked = model.mandatory
                                , label = Input.labelRight [] <| text "This identifier is mandatory"
                                }
                            ]
                        ]

                Format ->
                    column [ alignTop, spacing 10, width <| minimum 200 fill ]
                        [ h2 "Format:"
                        , wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                            List.append
                                (if List.isEmpty model.format then
                                    [ el [ padding 5, Font.color color.text.disabled ] (text "Empty") ]

                                 else
                                    []
                                )
                                (List.indexedMap
                                    (\i p ->
                                        row [ Background.color color.item.selected ]
                                            [ el [ padding 5 ] (text <| Portion.toString p)
                                            , button.secondary
                                                { onPress =
                                                    Just <|
                                                        InputFormat
                                                            (model.format
                                                                |> List.indexedMap Tuple.pair
                                                                |> List.filter (\( j, q ) -> j /= i)
                                                                |> List.map Tuple.second
                                                            )
                                                , label = text "×"
                                                }
                                            ]
                                    )
                                    model.format
                                )
                        , h2 "Construct the format of your identifier by clicking on the items below"
                        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ] <|
                            List.map
                                (\p ->
                                    column [ Background.color color.item.background, mouseOver itemHoverstyle, width (px 250), height (px 150) ]
                                        [ row [ alignLeft ]
                                            [ button.primary { onPress = Just <| InputFormat <| model.format ++ [ p ], label = text "+" }
                                            , el [ paddingXY 10 0 ] (text <| Portion.toString p)
                                            ]
                                        , paragraph [ padding 10, Font.size size.text.main ] [ text <| Portion.toDesc p ]
                                        ]
                                )
                                Portion.all
                        ]

                Name ->
                    el [ alignTop ] <|
                        Input.text
                            [ width <| minimum 200 fill
                            , Input.focusedOnLoad
                            , View.onEnter <|
                                case validate model of
                                    Ok f ->
                                        Added f

                                    Err err ->
                                        Warning ("Error: " ++ err)
                            ]
                            { onChange = InputName
                            , text = model.name
                            , placeholder =
                                Just <| Input.placeholder [] <| text "Name"
                            , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text "Give a name to this new identifier"
                            }
    in
    column [ width fill, alignTop, padding 20 ]
        [ column [ width fill, Border.shadow shadowStyle, padding 0, centerX, alignTop ]
            [ topbar "Adding an identifier"
            , column [ width fill, padding 20, centerX, alignTop, spacing 20 ]
                [ column
                    [ width fill, alignTop, spacing 20, padding 20 ]
                    (buttons
                        ++ [ el [ alignTop, alignLeft ] (h3 "Adding an identifier")
                           , step
                           ]
                    )
                ]
            ]
        ]
