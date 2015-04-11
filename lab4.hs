module RBTree where

import Prelude hiding (Left, Right)

data Context a = Top
               | Left a (Tree a) (Context a)
               | Right a (Tree a) (Context a)
data Color = Red | Black deriving (Eq, Show)
data Tree a = Empty | Node a (Tree a) (Tree a)
                 deriving (Eq, Show)
data Loc a = Loc (Tree a) (Context a)
data Map k v
data RBNode a = RBNode a Color
-- an Empty node is a leaf and is black

{-
A red black tree must have these conditions:
1. A node is either red or black
2. The root is black
3. All leaves(Empty) are black.
4. Every red node has two black child nodes.
5. Every path from the root to an Empty node contains
   the same number of black nodes.
-}

main :: IO()
main = putStrLn "Undefined"

--functions required to write
size :: Tree a -> Int
size Empty = 0
size (Node _ l r) = (size l) + 1 + (size r)

--zipper traversal
-- v - value, c - color, l - left child, r - right child
downLeft :: Loc a -> Maybe (Loc a)
downLeft (Loc (Node v l r) context) = Just $ Loc l (Left v r context)

downRight :: Loc a -> Maybe (Loc a)
downRight (Loc (Node v l r) context) = Just $ Loc r (Right v l context)



--helper functions
