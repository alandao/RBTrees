data Color = Red | Black deriving (Eq, Show)
data RBTree a = Empty | Node a Color (RBTree a) (RBTree a) deriving (Eq, Show)

main :: IO()
main = putStrLn "Undefined"

--helper functions
rbtLeftChild :: (Ord a) => RBTree a -> RBTree a
rbtLeftChild (Node _ _ l _) = l

rbtRightChild :: (Ord a) => RBTree a -> RBTree a
rbtRightChild (Node _ _ _ r) = r

rbtValue :: (Ord a) => RBTree a -> a
rbtValue (Node v _ _ _) = v

rbtColor :: (Ord a) => RBTree a -> Color
rbtColor (Node _ c _ _) = c
rbtColor Empty = Black
