module TreeDisplay where

import Graphics.Collage exposing (..)
import Tree exposing (..)
import Color
import Text
import Graphics.Element exposing (..)

displayTree: (Float, Float) -> Tree x -> List Form 
displayTree (x, y) tree =
  case tree of
    Empty ->
      []
    Node v left right ->
      displayTree (x - 20.0, y - 50.0) left ++ 
      [nodeForms v (x, y), nodeText v (x,y)] ++
      displayTree (x + 20.0, y - 50.0) right

      
nodeForms v (x,y) =
  circle 15.0 |> filled Color.red |> move (x,y)

nodeText v (x,y) =  
  v |> toString 
    |> Text.fromString 
    |> Text.color Color.white 
    |> text 
    |> move (x,y)

treeElement: List Int -> Element
treeElement treeList = 
  collage 300 300 (displayTree (0,100) (fromList treeList))
