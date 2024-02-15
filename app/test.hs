module Main where

import System.Random
import Data.List (nub, sortOn)
data Card = Card { mark :: Int, number :: Int } deriving (Eq, Ord, Show)
getNubRandomRs :: RandomGen g => g -> (Int, Int) -> [Int]
getNubRandomRs g (l, h) = take (abs (h - l) + 1) $ nub $ randomRs (l, h) g
shuffle :: RandomGen g => g -> [a] -> [a]
shuffle _ [] = []
shuffle g xs = fmap fst $ sortOn snd $ zip xs rs
    where
        rs = getNubRandomRs g (1, length xs)
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
preflopDrawNplayer :: Int -> ([[Card]], [Card]) -> ([[Card]], [Card])
preflopDrawNplayer 0 (hands, deck) = (hands, deck)
preflopDrawNplayer n (hands, deck) = preflopDrawNplayer (n-1) (hands ++ [fst (drawCard deck 2)], snd (drawCard deck 2))
preFlopInit :: Int -> IO ([[Card]], [Card])
preFlopInit n = do
  g <- getStdGen
  let shuffled_deck = shuffle g [Card x y | x <- [0..3], y <- [1..13]]
  print shuffled_deck
  let (hands, deck_after_preflop) = preflopDrawNplayer n ([], shuffled_deck)
  return (hands, deck_after_preflop)
main :: IO ()
main = do
    (hands, deck_after_preflop) <- preFlopInit 4
    print (hands, deck_after_preflop)  