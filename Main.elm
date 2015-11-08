module Main where
import Signal
import Graphics.Element exposing (..)
import Html exposing (input, toElement)
import Html.Attributes exposing (type', min, max, step, value)
import Html.Events exposing (on, targetValue)
import Graphics.Input exposing (button)
import Time exposing (..)
import String exposing (toFloat)

type TimerState = Paused | Running
type TimeControl = ResetTimer | SetTimer Int | PauseTimer | PlayTimer | TimerTick Float 

type alias Model =
  {
    state : TimerState
  , position : Int
  }

model : Model
model = 
  {
    state = Running,
    position = 0
  }

main : Signal Element
main = 
  Signal.map view 
    (Signal.foldp updateTimer model 
      (Signal.merge 
        timeControl.signal 
        (Signal.map TimerTick (every (1 * Time.millisecond)))))


timeControl : Signal.Mailbox TimeControl
timeControl = Signal.mailbox ResetTimer

updateTimer: TimeControl -> Model -> Model
updateTimer action model = 
  let 
    updatePosition model = 
      if model.state == Running  
        then (if model.position < 1000
                  then model.position + 1 
                  else 0)
        else model.position
  in
    case action of
      SetTimer v -> {model | position <- v }
      ResetTimer -> {model | position <- 0 }
      PauseTimer -> {model | state <- Paused}
      PlayTimer -> {model | state <- Running}
      TimerTick x -> {model | position <- (updatePosition model)}


view : Model -> Element
view timer = 
  let 
    position = timer.position
  in
    flow down 
      [
        show position
      , (toElement 1000 30 
          (Html.input 
            [(type' "range")
            , (Html.Attributes.min "0")
            , (Html.Attributes.max "1000")
            , (step "1")
            , (value (toString position))
            , on "input" targetValue sendTimeMessage] 
          [])
        )
      , flow up [
          button (Signal.message timeControl.address ResetTimer) "Reset" 
        , button (Signal.message timeControl.address PauseTimer) "Pause" 
        , button (Signal.message timeControl.address PlayTimer) "Play"
        ]
    ]

sendTimeMessage : String -> Signal.Message
sendTimeMessage value =
  let 
    valueAsInt = (stringToIntOrZero value)
  in
    (Signal.message timeControl.address (SetTimer valueAsInt))

stringToIntOrZero: String -> Int
stringToIntOrZero string = 
    String.toFloat string 
    |> Result.toMaybe 
    |> Maybe.withDefault 0
    |> round

