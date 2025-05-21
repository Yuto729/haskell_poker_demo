module Main where
import System.Random
import Poker
import Utils
main :: IO ()
main = do
  print "enter numbers of player"
  string <- getLine
  let numbers = stringToInt string
  if numbers > 10 || numbers < 2 then do
    print "invalid input"
    main
  else do
    g <- getStdGen
    let shuffled_players = shuffle g [0..numbers-1]
    playGame shuffled_players
