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

test_1 = insert (Map 49 "a") $ insert (Map 50 "a") Empty
test_2 = insert (Map 51 "a") $ insert (Map 50 "a") Empty

main :: IO()
main = putStrLn "Undefined"

--functions required to write
size :: Tree a -> Int
size Empty = 0
size (Node _ l r) = size l + 1 + size r

--Red Black Tree operations
insert :: (Ord a, Eq b) => Map a b -> RBTreeMap a b -> RBTreeMap a b
insert item tree = unzippify $ fixRBTree $ insertZipped item (Just (zippify tree))

insertZipped :: (Ord a, Eq b) => Map a b -> Maybe (Loc (RBNode (Map a b)))-> Loc (RBNode (Map a b))
insertZipped _ Nothing = error "insertZipped's insert operation is flawed."
insertZipped kv (Just (Loc Empty context)) = Loc (Node (RB kv Red) Empty Empty) context
insertZipped kv@(Map key value) zipTree@(Just (Loc (Node (RB (Map curKey _) _) l r)context))
  | key < curKey = insertZipped kv (zipTree >>= downLeft)
  | key > curKey = insertZipped kv (zipTree >>= downRight)
  | key == curKey = Loc (Node (RB (Map key value) Red) l r) context

--case 1: N is the root node
fixRBTree :: (Eq a) => Loc (RBNode a) -> Loc (RBNode a)
fixRBTree (Loc (Node (RB v Red) l r) Top) = Loc (Node (RB v Black) l r) Top
fixRBTree focus = fixRBTree_2 focus

--case 2: P is black, we good
fixRBTree_2 :: (Eq a) => Loc (RBNode a) -> Loc (RBNode a)
fixRBTree_2 focus
  | parentColor focus == Just Black = focus
  | otherwise = fixRBTree_3 focus

--case 3: P and U are both red.
fixRBTree_3 :: (Eq a) => Loc (RBNode a) -> Loc (RBNode a)
fixRBTree_3 focus
  | parentColor focus == Just Red && uncleColor focus == Just Red = fixRBTree $
                                                          setRed $
                                                          assertGrandparent $
                                                          setBlack $
                                                          assertSibling $
                                                          Just $
                                                          setBlack $
                                                          assertParent focus
  | otherwise = balance focus
  where setBlack focus = (\x -> setColorOfFocus x Black) focus
        setRed focus = (\x -> setColorOfFocus x Red) focus
        assertParent focus = case parent focus of
                             Just x-> x
                             Nothing -> error "fixRBTree_3's focus has no parent."
        assertSibling focus = case focus >>= sibling of
                             Just x -> x
                             Nothing -> error "fixRBTree_3's focus has no uncle."
        assertGrandparent focus = case parent focus of
                             Just x -> x
                             Nothing -> error "fixRBTree_3's focus has no grandparent."
{-
fixRBTree focus@(Loc _ _)
  | isNothing parent = error "fixRBTree has no parent! first pattern match failed"
  | getColorOfFocus (fromJust parent) == Black = focus
  | getColorOfFocus (fromJust parent) == Red && --beware, nasty stuff inbound
    getColorOfFocus (fromJust (getSibling parent)) == Red =  fixRBTree $
                                                             paintCurTreeRed $
                                                             fromJust ((Just $
                                                             paintCurTreeBlack $
                                                             fromJust $
                                                             getSibling $
                                                             Just $
                                                             paintCurTreeBlack $
                                                             fromJust parent)>>=
                                                             up)
  | otherwise = balance focus
    where parent = Just focus >>= up
          getSibling x = if locIsLeftNodeOfParent (fromJust x) --sibling of parent is uncle(or aunt)
                  then x >>= up >>= downRight
                  else x >>= up >>= downLeft
          paintCurTreeBlack = modifyLoc $ modifyRoot rbPaintItBlack
          paintCurTreeRed = modifyLoc $ modifyRoot rbPaintItRed
          getColorOfFocus = rbGetColor . fromJust . getValue . locGetCurTree
-}
balance :: (Eq a) => Loc (RBNode a) -> Loc (RBNode a)
balance focus
  | isLR = balance $ fromJust $ Just (rotateTreeAtFocus rotateLeft (fromJust (Just focus >>= up))) >>= downLeft
  | isRL = balance $ fromJust $ Just (rotateTreeAtFocus rotateRight (fromJust (Just focus >>= up))) >>= downRight
  | isLL = setRed $ fromJust $ Just (setBlack $ rotateTreeAtFocus rotateRight (fromJust $ grandParent focus)) >>= downRight
  | isRR = setRed $ fromJust $ Just (setBlack $ rotateTreeAtFocus rotateLeft (fromJust $ grandParent focus)) >>= downLeft
    where isLR = (grandParent focus >>= downLeft >>= downRight) == Just focus
          isRL = (grandParent focus >>= downRight >>= downLeft) == Just focus
          isLL = (grandParent focus >>= downLeft >>= downLeft) == Just focus
          isRR = (grandParent focus >>= downRight >>= downRight) == Just focus
          rotateTreeAtFocus f (Loc tree context) = Loc (f tree) context
          setBlack = (\x -> setColorOfFocus x Black)
          setRed = (\x -> setColorOfFocus x Red)

--nontotal function.
--variable names are in neal's lecture notes
rotateLeft :: Tree a -> Tree a
rotateLeft (Node p one (Node n two three)) = Node n (Node p one two) three

rotateRight :: Tree a -> Tree a
rotateRight (Node p (Node n three two) one) = Node n three (Node p two one)

--data Tree a = Node a (Tree a) (Tree a)
find :: (Ord a) => a -> Tree a -> Bool
find x Empty = False
find x (Node v l r)
  | x == v = True
  | x < v = find x l
  | x > v = find x r

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

--helper functions
setColorOfFocus :: Loc (RBNode a) -> Color -> Loc (RBNode a)
setColorOfFocus (Loc tree context) c = Loc (modifyRoot (\x -> rbPaint x c) tree) context

parentColor :: Eq a => Loc (RBNode a) -> Maybe Color
parentColor focus = case parent focus of
                    Just x -> Just $ colorOfFocus x
                    Nothing -> Nothing
uncleColor :: Eq a => Loc (RBNode a) -> Maybe Color
uncleColor focus = case uncle focus of
                   Just x -> Just $ colorOfFocus x
                   Nothing -> Just Black
nodeValueOfCurTree :: Loc a -> a
nodeValueOfCurTree x = case getNodeValue $ locGetCurTree x of
                        Just value -> value
                        Nothing -> error "Value is Empty"

parent :: Loc a -> Maybe (Loc a)
parent focus = Just focus >>= up

uncle :: Eq a => Loc a -> Maybe (Loc a)
uncle focus = (parent focus) >>= sibling

sibling :: Eq a => Loc a -> Maybe (Loc a)
sibling focus
  | ((Just focus >>= up >>= downLeft) == Just focus) = Just focus >>= up >>= downRight
  | otherwise = Just focus >>= up >>= downLeft

grandParent :: Loc a -> Maybe (Loc a)
grandParent focus = Just focus >>= up >>= up

colorOfFocus :: Loc (RBNode a) -> Color
colorOfFocus = rbGetColor . nodeValueOfCurTree

modifyRoot :: (a -> a) -> Tree a -> Tree a
modifyRoot f (Node v l r) = Node (f v) l r

zippify :: Tree a -> Loc a
zippify t = Loc t Top

getNodeValue :: Tree a -> Maybe a
getNodeValue (Node v _ _) = Just v
getNodeValue Empty = Nothing

unzippify :: Loc a -> Tree a
unzippify loc = locGetCurTree $ top loc

locGetCurTree :: Loc a -> Tree a
locGetCurTree (Loc tree _) = tree

modifyLoc :: (Tree a -> Tree a) -> Loc a -> Loc a
modifyLoc f (Loc tree context) = Loc (f tree) context

locIsLeftNodeOfParent :: Loc a -> Bool
locIsLeftNodeOfParent (Loc _ (Left{})) = True
locIsLeftNodeOfParent _ = False

rbGetColor :: RBNode a -> Color
rbGetColor (RB _ c) = c

rbGetValue :: RBNode a -> a
rbGetValue (RB value _) = value

rbPaint :: RBNode a -> Color -> RBNode a
rbPaint (RB v _) col = RB v col

mapGetValue :: Map a b -> b
mapGetValue (Map _ v) = v

mapGetKey :: Map a b -> a
mapGetKey (Map k _) = k

