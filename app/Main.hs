module Main where

import System.Random 
import Data.List (nub, sortOn)
data Card = Card { mark :: Char, number :: Int } deriving Show

getNubRandomRs :: RandomGen g => g -> (Int, Int) -> [Int]
getNubRandomRs g (l, h) = take (abs (h - l) + 1) $ nub $ randomRs (l, h) g
shuffle :: RandomGen g => g -> [a] -> [a]
shuffle _ [] = []
shuffle g xs = fmap fst $ sortOn snd $ zip xs rs
    where
        rs = getNubRandomRs g (1, length xs)
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
exchangeCards :: [Int] -> [Card] -> [Card] -> [Card]
exchangeCards [] hands _ = hands
exchangeCards _ hands [] = hands
exchangeCards (head:selected) hands (top:cards) = exchangeCards selected (replace hands (head, top)) cards
main :: IO ()
main = do
    let cards = [Card x y | x <- ['S', 'H', 'D', 'K'], y <- [1..13]]
    g <- getStdGen
    let shuffled_cards = shuffle g cards
    let inithand = take 5 shuffled_cards
    let shuffled_cards2 = removeFromList 5 shuffled_cards
    putStrLn "your initial hand is"
    print inithand
    putStrLn "select cards index/-1"
    string <- getLine
    let selected_list = stringToIntList string
    let afterExchangeCard = exchangeCards selected_list inithand shuffled_cards2
    print afterExchangeCard