data Color = Red | Black deriving (Eq, Show)
data RBTree a = Empty | Node a Color (RBTree a) (RBTree a) deriving (Eq, Show)

main :: IO()
main = putStrLn "Undefined"

