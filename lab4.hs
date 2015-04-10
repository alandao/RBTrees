data Color = Red | Black deriving (Eq, Show)
data RBTreeMap k v = Empty | Node k v Color (RBTreeMap k v) (RBTreeMap k v)
                 deriving (Eq, Show)
--an Empty node is a leaf and is black

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
tree2 = Node "blah" 10 Black Empty Empty

main :: IO()
main = putStrLn "Undefined"

--functions required to write
size :: RBTreeMap k v -> Int
size Empty = 0
size (Node _ _ _ l r) = (size l) + 1 + (size r)

--helper functions
leftChild :: (Ord k) => RBTreeMap k v -> RBTreeMap k v
leftChild (Node _ _ _ l _) = l

rightChild :: (Ord k) => RBTreeMap k v -> RBTreeMap k v
rightChild (Node _ _ _ _ r) = r

key :: (Ord k) => RBTreeMap k v -> k
key (Node k _ _ _ _) = k

value :: (Ord k) => RBTreeMap k v -> v
value (Node _ v _ _ _) = v

bstInsert :: (Ord k) => RBTree k v -> k -> v -> RBTree k v
bstInsert (Node k v c l r) key value
  | key < k = bstInsert l 
