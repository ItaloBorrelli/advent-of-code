import System.IO ()

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap :: (a -> b) -> Pair a -> Pair b
    fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
    pure :: a -> Pair a
    pure a = Pair a a
    (<*>) :: Pair (a -> b) -> Pair a -> Pair b
    (Pair f g) <*> (Pair a b) = Pair (f a) (g b)

instance Monad Pair where
    return :: a -> Pair a
    return = pure
    (>>=) :: Pair a -> (a -> Pair b) -> Pair b
    (Pair a b) >>= f =  let Pair a' _ = f a 
                            Pair _ b' = f b
                        in  Pair a' b'

split' :: Eq a => a -> [a] -> Pair [a]
split' _ [] = Pair [] []
split' d (x:xs)
    | x == d    = Pair [] xs
    | otherwise = Pair (x:) id <*> split' d xs

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
    contents <- readFile "2022/04.txt"
    let x = Pair 1 1 :: Pair Integer
    let y = fmap (+ 1) x
    print $ map (fmap (fmap readInt . split' '-') . split' ',') $ lines contents
    print y
