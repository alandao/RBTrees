data Color = Red | Black deriving (Eq, Show)
data RBTreeMap k v = Empty | Node k v Color (RBTreeMap k v) (RBTreeMap k v)
                 deriving (Eq, Show)
-- an Empty node is a leaf and is black
data NodeAncestors = RBTreeMap RBTreeMap RBTreeMap 
--                    Parent   GrandDad    Uncle

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

bstInsert :: (Ord k) => RBTreeMap k v -> k -> v ->
          NodeAncestors -> (RBTreeMap k v, NodeAncestors)
bstInsert Empty key value = Node key value Red Empty Empty
bstInsert t@(Node k v c l r) key value anc
  | key < k = (Node k v c (bstInsert l key value) r, t Empty r)
  | key > k = (Node k v c l (bstInsert r key value), t Empty l)
  | key == k = (Node k value Red l r, anc)

fix :: RBTreeMap k v -> RBTreeMap k v
