module RBTrees where

import Prelude hiding (Left, Right)
data Color = Red | Black deriving (Eq, Show)
data RBTree a = Empty | Node a Color (RBTree a) (RBTree a)
                 deriving (Eq, Show)
data Map k v
data Loc a = Loc (RBTree a) (Context a)
data Context a = Top
               | Left a (RBTree a) (Context a)
               | Right a (RBTree a) (Context a)
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
tree1 = Empty
tree2 = Node 5 "person" Black Empty Empty

main :: IO()
main = putStrLn "Undefined"

--functions required to write
size :: RBTree a -> Int
size Empty = 0
size (Node _ _ l r) = (size l) + 1 + (size r)

--zipper traversal
downLeft :: Loc a -> Loc a
downLeft Loc (Node k v c l r) context = Loc l (Left k v c r context)

up :: Loc a -> Loc a
up Loc t (Left k v c r context) = Loc (Node k v c)

--helper functions
