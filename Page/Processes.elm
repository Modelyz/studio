module Page.Processes exposing (..)

import REA.Process exposing (Process)
import Msg


viewProcesses : List Process -> Html Msg.Msg
    div
        [ class "section"
        ]
        [ button
            [ onClick <| Msg.NewProcess
            , class "button"
            ]
            [ text "New pizza sale"
            ]
        , div [class "columns", class "is-multiline"]
              <| List.map Page.Process.viewThumbnail model.processes
        ]
