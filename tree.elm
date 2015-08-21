module Tree where

type Tree a
    = Empty
    | Node a (Tree a) (Tree a)

empty : Tree a
empty =
    Empty

singleton : a -> Tree a
singleton v =
    Node v Empty Empty

insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case tree of
      Empty ->
          singleton x

      Node y left right ->
          if  | x > y -> Node y left (insert x right)
              | x < y -> Node y (insert x left) right
              | otherwise -> tree

fromList : List comparable -> Tree comparable
fromList xs =
    List.foldl insert empty xs


