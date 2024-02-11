removeFromList :: Int -> [b] -> [b]
removeFromList _ [] = []
removeFromList 0 xs = xs
removeFromList n (x:xs) = removeFromList (n-1) xs
replace :: [a] -> (Int, a) -> [a]
replace xs (i, e) = before ++ [e] ++ after
  where
    (before, _:after) = splitAt i xs
exchangeCards :: [Int] -> [Int] -> [Int] -> [Int]
exchangeCards [] hands _ = hands
exchangeCards _ hands [] = hands
exchangeCards (head:selected) hands (top:cards) = exchangeCards selected (replace hands (head, top)) cards
main :: IO ()
main = do
    let selected = [0, 1]
    let hands = [1, 2, 3, 4, 5]
    let cards = [0, 15, 12, 3, 4]
    let hand2 = exchangeCards selected hands cards
    print hand2