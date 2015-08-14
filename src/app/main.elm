import Html exposing (Html, p, h3, div, button, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Signal exposing (..)
import Time exposing (fps)
import String exposing (padLeft)


-- MODEL

type alias TimeStamp = {
    index: Int,
    milliseconds: Float
}

type alias StopwatchModel = {
    started: Bool,
    paused: Bool,
    milliseconds: Float,

    timeStamps: List TimeStamp 
}



-- UPDATES
type Action =  NoOp
              |Start
              |Stop
              |Pause
              |Resume
              |AddTimeStamp
              |UpdateTime Float

update: Action -> StopwatchModel -> StopwatchModel 
update action model = 
    case action of
        Start           -> { model | 
                                started <- True,
                                paused <- False
                           }
        Stop            -> { model |
                                started <- False,
                                paused <- False,
                                milliseconds <- 0,
                                timeStamps <- []
                           }
        AddTimeStamp    -> 
                        let
                            index = (List.length model.timeStamps) 
                            currentms = model.milliseconds
                            newStamp = {
                                index = index,
                                milliseconds =  currentms
                            }
                        in  
                            { model | timeStamps <- newStamp :: model.timeStamps }

        Pause           -> { model | paused <- True }
        Resume          -> { model | paused <- False }

        UpdateTime dt   -> if (model.started && not model.paused) then 
                                { model | milliseconds <- (model.milliseconds + dt )}
                           else 
                                model




-- VIEW


view : Signal.Address Action -> StopwatchModel -> Html.Html
view address model = 
    div [ class "stopwatch__container" ] [

        div [ class "stopwatch" ] [
            h3 [ class "stopwatch__caption" ] [
                text "Stopwatch"
            ],

            p [ class "stopwatch__time" ] [
                text (timeStringFromMs model.milliseconds)
            ],

            controlsView address model,

            if List.length model.timeStamps > 0 then
                timeStampsView address model
            else div [] []
        ]
    ]


timeStringFromMs: Float -> String
timeStringFromMs ms = 
             let 
                centiSeconds = (truncate (ms / 10)) % 100
                seconds =   truncate (ms / 1000)
                minutes =   seconds // 60
                hours =     minutes // 60

                secondsMod = seconds % 60
                minutesMod = minutes

                centiString =   centiSeconds |> toString |> padLeft 2 '0'
                secondsString = secondsMod |> toString |> padLeft 2 '0'
                minutesString = minutesMod |> toString |> padLeft 2 '0'

             in 
                minutesString ++ ":" ++ secondsString ++ "." ++ centiString



controlsView: Signal.Address Action -> StopwatchModel -> Html.Html
controlsView address model =
    let
        showStart   =   (not model.started) && (not model.paused)
        showStop    =   (model.started) || (model.paused)
        showResume  =   model.paused
        showPause   =   (model.started) && (not model.paused)
        showAddStamp =  model.started
    in 

    div [ class "stopwatch__controls"] [
        button [ 
            classList [
                ("stopwatch__controls__start animate-width", True),
                ("hidden-animate-width", not showStart)
            ],
            onClick address Start
        ][
            text "Start"
        ],

        button [ 
            classList [
                ("stopwatch__controls__stop animate-width", True),
                ("hidden-animate-width", not showStop)
            ],
            onClick address Stop
        ][
            text "Stop"
        ],

        button [ 
            classList [
                ("stopwatch__controls__pause animate-width", True),
                ("hidden-animate-width", not showPause)
            ],
            onClick address Pause
        ][
            text "Pause"
        ],

        button [
            classList [
                ("stopwatch__controls__resume animate-width", True),
                ("hidden-animate-width", not showResume)
            ],
            onClick address Resume
        ][
            text "Resume"
        ],

        button [
            classList [
                ("stopwatch__controls__stamp animate-width", True),
                ("hidden-animate-width", not showAddStamp)
            ],
            onClick address AddTimeStamp
        ][
            text "Stamp"
        ]
    ]


timeStampsView : Signal.Address Action -> StopwatchModel -> Html.Html
timeStampsView address model = 
    let
        renderTimeStamp: TimeStamp -> TimeStamp -> Html.Html
        renderTimeStamp prevStamp stamp =
            div [class "stopwatch__timeStamps__stamp" ][
                div [class "stopwatch__timeStamps__stamp__index" ][
                    text ("#" ++ (toString stamp.index) )
                ],
                div [class "stopwatch__timeStamps__stamp__difference" ][
                    text (timeStringFromMs (stamp.milliseconds - prevStamp.milliseconds ))
                ],
                div [class "stopwatch__timeStamps__stamp__absolute" ][
                    text  (timeStringFromMs stamp.milliseconds)
                ]
            ]

        pendingStamp = {
            index = List.length model.timeStamps,
            milliseconds = model.milliseconds
        }

        nullStamp = {
            index =        -1,
            milliseconds =  0
        }

    in
        div [ class "stopwatch__timeStamps"] (
            List.map2 renderTimeStamp (model.timeStamps ++ [nullStamp] ) (pendingStamp :: model.timeStamps)
        )




-- WIRING UP

main : Signal Html
main = view actions.address <~ model


model : Signal StopwatchModel
model =
    Signal.foldp update initialModel (Signal.merge actions.signal updateTimeSignal)


updateTimeSignal : Signal Action
updateTimeSignal = 
              UpdateTime <~ (fps 31)


initialModel : StopwatchModel
initialModel = {
                   started = False,
                   paused = False,
                   milliseconds = 0,
                   timeStamps = [ ]
               }


actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp





