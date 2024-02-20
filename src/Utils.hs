module Utils where

    {-
  Queueの定義
-}
type Queue a = ([a], [a])

empty :: Queue a
empty = ([], [])

check :: [a] -> [a] -> Queue a
check [] r = (reverse r, [])
check f  r = (f, r)

snoc :: Queue a -> a -> Queue a
snoc (f, r) x = check f (x : r)

front :: Queue a -> a
front ([], _) = error "empty queue"
front (x : _, _) = x

pop :: Queue a -> Queue a
pop ([], _) = error "empty queue"
pop (_ : f, r) = check f r

stringToInt :: String -> Int
stringToInt s = read s

stringToIntList :: String -> [Int]
stringToIntList s = map read $ words s

removeFromList :: Int -> [a] -> [a]
removeFromList _ [] = []
removeFromList 0 xs = xs
removeFromList n (_:xs) = removeFromList (n-1) xs

replace :: [a] -> (Int, a) -> [a]
replace xs (i, e) = before ++ [e] ++ after
  where
    (before, _:after) = splitAt i xs

findListIndex :: (a -> Bool) -> [a] -> [Int]
findListIndex predicate xs = map (fst) (filter (predicate . snd) (zip [0..] xs))

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = (f a) && (g a)