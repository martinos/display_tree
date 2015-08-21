module TreeDisplay where

import Graphics.Collage exposing (..)
import Tree exposing (..)
import Color
import Text
import Graphics.Element exposing (..)
import Debug

displayTree: (Float, Float) -> (Float, Float) -> Int -> Tree x -> List Form 
displayTree parentCoor (x, y) depth tree =
  case tree of
    Empty ->
      []
    Node v left right ->
      let 
          height = 25.0 
          origWidth = 50.0
          newWidth = origWidth * (5.0 - toFloat depth)  / 5.0 
          leftnodeCoor = (x - newWidth, y - height)
          rightnodeCoor = (x + newWidth, y - height)
      in
        displayTree (x, y) leftnodeCoor (depth + 1) left 
        ++ [nodeForms v parentCoor (x, y), nodeText v (x,y)] 
        ++ displayTree (x, y) rightnodeCoor (depth + 1) right

      
nodeForms v parentCoor (x,y) =
  group [ circle 15.0 |> filled Color.red |> move (x,y)
        , segment parentCoor (x,y) |> traced defaultLine ]


nodeText v (x,y) =  
  v |> toString 
    |> Text.fromString 
    |> Text.color Color.white 
    |> text 
    |> move (x,y)

treeElement: List Int -> Element
treeElement treeList = 
  collage 300 300 (displayTree (0,100) (0,100) 0 (fromList treeList))
