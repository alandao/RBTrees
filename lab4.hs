module RBTree where

import Prelude hiding (Left, Right)
import Data.Maybe

data Tree a = Empty | Node a (Tree a) (Tree a)
                 deriving (Eq, Show)
data Context a = Top
               | Left a (Tree a) (Context a)
               | Right a (Tree a) (Context a)
                 deriving (Show, Eq)
data Loc a = Loc (Tree a) (Context a)
             deriving (Show, Eq)
data Map k v = Map k v
               deriving (Show, Eq)
data Color = Red | Black deriving (Eq, Show)
data RBNode a = RB a Color
                deriving (Show, Eq)
type RBTreeMap k v = Tree (RBNode (Map k v))

-- an Empty node is a black leaf

{-
A red black tree must have these conditions:
1. A node is either red or black
2. The root is black
3. All leaves(Empty) are black.
4. Every red node has two black child nodes.
5. Every path from the root to an Empty node contains
   the same number of black nodes.
-}

tree1 :: RBTreeMap String Integer
tree1 = Node (RB (Map "george" 17) Black) Empty Empty

main :: IO()
main = putStrLn "Undefined"

--functions required to write
size :: Tree a -> Int
size Empty = 0
size (Node _ l r) = size l + 1 + size r

--Red Black Tree operations
insert :: (Ord a, Eq b) => Map a b -> RBTreeMap a b -> RBTreeMap a b
insert item tree = unzippify $ insertZipped item (Just (zippify tree))

insertZipped :: (Ord a, Eq b) => Map a b -> Maybe (Loc (RBNode (Map a b)))-> Loc (RBNode (Map a b))
insertZipped _ Nothing = error "this wasn't meant to happen, fix me!"
insertZipped kv (Just (Loc Empty context)) = Loc (Node (RB kv Red) Empty Empty) context
insertZipped kv@(Map key value) zipTree@(Just (Loc (Node (RB (Map curKey _) _) l r)context))
  | key < curKey = insertZipped kv (zipTree >>= downLeft)
  | key > curKey = insertZipped kv (zipTree >>= downRight)
  | key == curKey = (Loc (Node (RB (Map key value) Red) l r) context)

fixRBTree :: Loc (RBNode a) -> Loc (RBNode a)
--case 1: N is the root node
fixRBTree (Loc (Node (RB v Red) l r) Top) = Loc (Node (RB v Black) l r) Top
--case 2: P is black, we good
fixRBTree focus@(Loc _ context)
  | isNothing parent = error "fixRBTree has no parent! first pattern match failed"
  | getColorOfFocus (fromJust parent) == Black = focus
  | getColorOfFocus (fromJust parent) == Red && False = undefined
    where parent = Just focus >>= up
          getColorOfFocus = rbGetColor . fromJust . getValue . locGetCurTree

find :: a -> RBTreeMap a b -> b
find = undefined

--zippers
downLeft :: Loc a -> Maybe (Loc a)
downLeft (Loc (Node v l r) context) = Just $ Loc l (Left v r context)
downLeft _ = Nothing

downRight :: Loc a -> Maybe (Loc a)
downRight (Loc (Node v l r) context) = Just $ Loc r (Right v l context)
downRight _ = Nothing

up :: Loc a -> Maybe (Loc a)
up (Loc lTree (Left v rTree c)) = Just $ Loc (Node v lTree rTree) c
up (Loc rTree (Right v lTree c)) = Just $ Loc (Node v lTree rTree) c
up (Loc _ Top) = Nothing

top :: Loc a -> Loc a
top loc@(Loc _ Top) = loc
top (Loc lTree (Left v rTree c)) = top (Loc (Node v lTree rTree) c)
top (Loc rTree (Right v lTree c)) = top (Loc (Node v lTree rTree) c)

modify :: (Tree a -> Tree a) -> Loc a -> Loc a
modify f (Loc tree context) = Loc (f tree) context

--helper functions
rbGetColor :: RBNode a -> Color
rbGetColor (RB _ c) = c

rbGetValue :: RBNode a -> a
rbGetValue (RB value _) = value

zippify :: Tree a -> Loc a
zippify t = Loc t Top

unzippify :: Loc a -> Tree a
unzippify loc = locGetCurTree $ top loc

getValue :: Tree a -> Maybe a
getValue (Node v _ _) = Just v
getValue Empty = Nothing

mapGetValue :: Map a b -> b
mapGetValue (Map _ v) = v

mapGetKey :: Map a b -> a
mapGetKey (Map k _) = k

locGetCurTree :: Loc a -> Tree a
locGetCurTree (Loc tree _) = tree
