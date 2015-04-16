import Prelude hiding (Left, Right)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text


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

main :: IO()
main = do
  listOfTuple <- fmap (fmap (listToTuple . splitNameAndScore) . Text.lines) (Text.readFile "players_homeruns.csv")
  let tree = foldr (\x -> insert $ Map (fst x) (snd x)) Empty listOfTuple
  menu tree
    where splitNameAndScore = Text.splitOn $ Text.pack ","
          listToTuple [x,y] = (x,y)

menu :: (Show b) => RBTreeMap Text.Text b -> IO()
menu tree = do
  putStrLn "Enter a player name or \"ALL\""
  input <- getLine
  case input of
   "ALL" -> Text.putStrLn $ foldr (<>) (Text.pack "") (fmap ((<> Text.pack "\n"). Text.pack . show) $ keySet tree)
   _ -> case (find (Text.pack input) tree) of
         Just homeruns -> Text.putStrLn ((Text.pack . show) homeruns)
         Nothing -> putStrLn "Player not found."
  menu tree

showTree :: Show a => Tree a -> Text.Text
showTree Empty = Text.pack ""
showTree (Node v l r)= showTree l <> (Text.pack $ show v) <> (Text.pack "\n") <> showTree r

--functions required to write
insert :: (Ord a, Eq b) => Map a b -> RBTreeMap a b -> RBTreeMap a b
insert item tree = unzippify $ fixRBTree $ insertZipped item (Just (zippify tree))

find :: Ord a => a -> RBTreeMap a b -> Maybe b
find _ Empty = Nothing
find x (Node (RB (Map key value) _) l r)
  | x == key = Just value
  | x < key = find x l
  | x > key = find x r

containsKey :: (Ord k) => k -> RBTreeMap k v -> Bool
containsKey _ Empty = False
containsKey x (Node (RB (Map key _) _) l r)
  | x == key = True
  | x < key = containsKey x l
  | x > key = containsKey x r

count :: Tree a -> Int
count Empty = 0
count (Node _ l r) = count l + 1 + count r

keySet :: RBTreeMap k v -> [k]
keySet Empty = []
keySet (Node (RB (Map key _) _) l r) = keySet l ++ [key] ++ keySet r

--Red Black Tree operations

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
  | isNothing (uncle focus) = balance focus
  | (parentColor focus == Just Red && uncleColor focus == Just Red) = fixRBTree $
                                                          setRed $
                                                          assertGrandparent $
                                                          setBlack $
                                                          assertSibling $
                                                          Just $
                                                          setBlack $
                                                          assertParent focus
  | otherwise = balance focus
  where setBlack x = setColorOfFocus x Black
        setRed x = setColorOfFocus x Red
        assertParent focus = case parent focus of
                             Just x-> x
                             Nothing -> error "fixRBTree_3's focus has no parent."
        assertSibling focus = case focus >>= sibling of
                             Just x -> x
                             Nothing -> error "fixRBTree_3's focus has no uncle."
        assertGrandparent focus = case parent focus of
                             Just x -> x
                             Nothing -> error "fixRBTree_3's focus has no grandparent."

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
          setBlack x = setColorOfFocus x Black
          setRed x = setColorOfFocus x Red

--nontotal function.
--variable names are in neal's lecture notes
rotateLeft :: Tree a -> Tree a
rotateLeft (Node p one (Node n two three)) = Node n (Node p one two) three

rotateRight :: Tree a -> Tree a
rotateRight (Node p (Node n three two) one) = Node n three (Node p two one)


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
                   Just x -> case getNodeValue (locGetCurTree x) of
                              Nothing -> Nothing
                              Just _ -> Just $ colorOfFocus x
                   Nothing -> Nothing

nodeValueOfCurTree :: Loc a -> a
nodeValueOfCurTree x = case getNodeValue $ locGetCurTree x of
                        Just value -> value
                        Nothing -> error "Value is Empty"

parent :: Loc a -> Maybe (Loc a)
parent focus = Just focus >>= up

uncle :: Eq a => Loc a -> Maybe (Loc a)
uncle focus = parent focus >>= sibling

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
rbPaint (RB v _) = RB v

mapGetValue :: Map a b -> b
mapGetValue (Map _ v) = v

mapGetKey :: Map a b -> a
mapGetKey (Map k _) = k

