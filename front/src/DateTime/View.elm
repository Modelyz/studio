module DateTime.View exposing (Model, Msg, init, inputDate, toPosix, update)

import Calendar
import Date exposing (Date)
import DateTime
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Shared
import Task
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix)
import View exposing (button)
import View.Style exposing (color, isMobile, shadowStyle, size)


type alias Model =
    { date : Date
    , zone : Time.Zone
    , ms : Int
    }


type Msg
    = GetCurrentDate
    | GotCurrentDate Date
    | InputDate Date
    | PrevMonth
    | NextMonth
    | PrevYear
    | NextYear


toPosix : Model -> Posix
toPosix model =
    millisToPosix <| model.ms + Date.diff Date.Days (Date.fromPosix model.zone (millisToPosix 0)) model.date * 24 * 60 * 60 * 1000


init : Bool -> Time.Zone -> Date -> ( Model, Cmd Msg )
init isNew zone date =
    ( { date = date
      , zone = zone
      , ms = 0
      }
    , if isNew then
        Task.perform GotCurrentDate Date.today

      else
        Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetCurrentDate ->
            ( model, Task.perform GotCurrentDate Date.today )

        GotCurrentDate now ->
            ( { model | date = now }, Cmd.none )

        InputDate date ->
            ( { model | date = date }, Cmd.none )

        PrevMonth ->
            ( { model | date = Date.add Date.Months -1 model.date }, Cmd.none )

        NextMonth ->
            ( { model | date = Date.add Date.Months 1 model.date }, Cmd.none )

        PrevYear ->
            ( { model | date = Date.add Date.Years -1 model.date }, Cmd.none )

        NextYear ->
            ( { model | date = Date.add Date.Years 1 model.date }, Cmd.none )


inputDate : Shared.Model -> Model -> Element Msg
inputDate s model =
    let
        mediumfont =
            if isMobile s.windowSize then
                size.text.small

            else
                size.text.main

        xlfont =
            if isMobile s.windowSize then
                size.text.main

            else
                size.text.xl

        d =
            if isMobile s.windowSize then
                5

            else
                10

        weekdayWidth =
            if isMobile s.windowSize then
                px 130

            else
                px 300

        dayWidth =
            if isMobile s.windowSize then
                px 130

            else
                px 75

        monthWidth =
            if isMobile s.windowSize then
                px 200

            else
                px 300

        yearWidth =
            monthWidth

        colwidth =
            if isMobile s.windowSize then
                px 15

            else
                px 30
    in
    column [ width fill ]
        [ wrappedRow [ Font.size xlfont, centerX, spacing 5 ]
            [ button.special (Ok GetCurrentDate) "Today"
            , el
                [ width weekdayWidth
                , Font.center
                , Background.color color.item.background
                , padding d
                ]
                (text <| DateTime.weekdayToLongString (Date.weekday model.date))
            , el [ width dayWidth, Font.center, Background.color color.item.background, padding d ] <| text <| String.fromInt (Date.day model.date)
            , row [ spacing 20, Background.color color.item.background, padding d ]
                [ el [ alpha 0.2, mouseOver [ alpha 0.6 ], pointer, onClick PrevMonth ] (text "◂")
                , el [ width monthWidth, Font.center ] (text <| DateTime.monthToLongString (Date.month model.date))
                , el [ alpha 0.2, mouseOver [ alpha 0.6 ], pointer, onClick NextMonth ] (text "▸")
                ]
            , row [ spacing 20, Background.color color.item.background, padding d ]
                [ el [ alpha 0.2, mouseOver [ alpha 0.6 ], pointer, onClick PrevYear ] (text "◂")
                , el [ width yearWidth, Font.center ] (text <| String.fromInt (Date.year model.date))
                , el [ alpha 0.2, mouseOver [ alpha 0.6 ], pointer, onClick NextYear ] (text "▸")
                ]
            ]

        --, el [ alignRight ] (text <| DateTime.zoneNameToString model.zonename)
        , column
            [ width fill, padding d, spacing d ]
          <|
            (row [ width fill, spacing d ] <|
                el [ width colwidth, Font.alignRight ] (text " ")
                    :: List.map (el [ Font.size mediumfont, width fill, spacing d, padding d ] << text << DateTime.weekdayToString << Date.numberToWeekday) (List.range 1 7)
            )
                :: List.map
                    (\calweek ->
                        row [ width fill, spacing d, Font.color color.text.light ] <|
                            (el [ width colwidth, Font.alignRight, Font.size mediumfont ] <| text <| Maybe.withDefault "" <| Maybe.map (String.fromInt << Date.weekNumber << .date) <| List.head calweek)
                                :: List.map
                                    (\calday ->
                                        row
                                            [ if Date.day model.date == Date.day calday.date then
                                                mouseOver [ Background.color color.item.selected ]

                                              else
                                                mouseOver [ Border.shadow shadowStyle ]
                                            , padding d
                                            , width fill
                                            , if model.date == calday.date then
                                                Background.color color.item.selected

                                              else
                                                Background.color color.item.background
                                            , onClick (InputDate calday.date)
                                            , pointer
                                            ]
                                            [ el
                                                [ Font.size xlfont
                                                , Font.color <|
                                                    if Date.month model.date == Date.month calday.date then
                                                        color.text.main

                                                    else
                                                        color.text.disabled
                                                ]
                                                (text <| String.fromInt <| Date.day calday.date)
                                            , if isMobile s.windowSize then
                                                none

                                              else
                                                column
                                                    [ alignRight
                                                    , spacing d
                                                    , Font.color <|
                                                        if Date.month model.date == Date.month calday.date then
                                                            color.text.light

                                                        else
                                                            color.text.disabled
                                                    , alpha 0.5
                                                    , mouseOver [ alpha 1 ]
                                                    ]
                                                    [ text <| DateTime.monthToString <| Date.month calday.date
                                                    , text <| String.fromInt <| Date.year calday.date
                                                    ]
                                            ]
                                    )
                                    calweek
                    )
                    (Calendar.fromDate (Just <| Calendar.Config Time.Mon) model.date)
        ]
