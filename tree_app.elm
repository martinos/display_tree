import Signal
import String
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (on, onClick, targetValue)
import TreeDisplay exposing (..)

-- Model --

type alias Model = 
  { input: String
  , nodes: List Int}

initModel: Model
initModel = {input = "", nodes = [5]}

-- Controller --

type Action
  = NoOp
  | Add
  | Update String

update: Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Update content -> 
      {model | input <- content}
    Add ->
      let 
        clearInput model =
          {model| input <- ""}
      in
        {model | nodes <- model.nodes ++ [(parseInt model.input)]} |> clearInput

-- View --

inputView address inputValue = 
  div [] 
      [ input [ value inputValue
                   , onInput address Update] 
                   []
      , button [onClick address Add] [text "Add"]]

view address model = 
  div []
    [ inputView app.address model.input
    , fromElement(treeElement model.nodes)]

-- The App --

app = Signal.mailbox NoOp

main = Signal.foldp update initModel app.signal |> Signal.map (view app.address)

-- Utils  -- 

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
      on "input" targetValue (\v -> Signal.message address (f v))

parseInt : String -> Int
parseInt string =
  case String.toInt string of
    Ok value ->
      value
    Err error ->
      0

