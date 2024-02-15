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
convertCardToShow ::  [String] -> [String] -> Card -> String
convertCardToShow suit_list number_list x = suit_list !! mark x ++ number_list !! number x
convertHandsToShow :: [String] -> [String] -> [Card] -> [String]
convertHandsToShow suit number hand = map (convertCardToShow suit number) hand
countSuits :: [Int] -> [Card] -> [Int]
countSuits xs [] = xs
countSuits xs (x:hand) = countSuits (replace xs (mark x, (xs !! mark x)+1)) hand
countNumber :: [Int] -> [Card] -> [Int]
countNumber xs [] = xs
countNumber xs (x:hand) = countNumber (replace xs (number x, (xs !! number x)+1)) hand
isOnePair :: [Int] -> Bool
isOnePair numbers = length (filter (==2) numbers) == 1
isTwoPair :: [Int] -> Bool
isTwoPair numbers = length (filter (==2) numbers) == 2
isThreeCard :: [Int] -> Bool
isThreeCard numbers = length (filter (==2) numbers) == 0 && length (filter (==3) numbers) == 1
isFourCard :: [Int] -> Bool
isFourCard numbers = length (filter (==4) numbers) == 1
isFullHouse :: [Int] -> Bool
isFullHouse numbers = length (filter (==2) numbers) == 1 && length (filter (==3) numbers) == 1
isFlash :: [Int] -> Bool
isFlash suits = length (filter (==5) suits) == 1
isStraight :: [Int] -> Bool
isStraight [] = False
isStraight (x:numbers) = x==1 && head numbers == 1 && head (tail numbers) == 1 && head (removeFromList 2 numbers) == 1 && head (removeFromList 3 numbers) == 1 || isStraight numbers
isStraightFlash :: [Int] -> [Int] -> Bool
isStraightFlash suits numbers= isFlash suits && isStraight numbers
isRoyalStraightFlush :: [Int] -> [Int] -> Bool
isRoyalStraightFlush suits numbers = isFlash suits && numbers !! 10 == 1 && numbers !! 11 == 1 && numbers !! 12 == 1 && numbers !! 13 == 1 && numbers !! 1 == 1
judge :: [Int] -> [Int] -> Int
judge suits numbers
  | isRoyalStraightFlush suits numbers = 9
  | isStraightFlash suits numbers = 8
  | isFourCard numbers = 7
  | isFullHouse numbers = 6
  | isFlash suits = 5
  | isStraight numbers = 4
  | isThreeCard numbers = 3
  | isTwoPair numbers = 2
  | isOnePair numbers = 1
  | otherwise = 0
betAction :: [Int] -> IO [Int]
betAction pre_bet_amount_list = do
  if head pre_bet_amount_list == -1
    then return pre_bet_amount_list
  else do
    print "select action (-1: fold, 0: check, 1: call, 2: raise)"
    action_string <- getLine
    let action_int = stringToInt action_string
    if action_int == -1 then do
      return (-1: tail pre_bet_amount_list)
    else if action_int == 0 then do
      return pre_bet_amount_list
    else if action_int == 1 then do
      if head pre_bet_amount_list == maximum pre_bet_amount_list then do 
        print "you can't call. select other action"
        betAction pre_bet_amount_list
      else do
        return (maximum pre_bet_amount_list: tail pre_bet_amount_list)
    else if action_int == 2 then do
      print "enter bet amount"
      bet_amount <- getLine
      let bet_amount_int = stringToInt bet_amount
      if bet_amount_int <= 0 then do
        print "invalid input"
        betAction pre_bet_amount_list
      else do
        return ((head pre_bet_amount_list + bet_amount_int): tail pre_bet_amount_list)
    else do
      print "invalid input."
      betAction pre_bet_amount_list
drawCard :: [Card] -> Int -> ([Card], [Card])
drawCard deck n = (take n deck, removeFromList n deck)
preflopDrawNplayer :: Int -> ([[Card]], [Card]) -> ([[Card]], [Card])
preflopDrawNplayer 0 (hands, deck) = (hands, deck)
preflopDrawNplayer n (hands, deck) = preflopDrawNplayer (n-1) (hands ++ [fst (drawCard deck 2)], snd (drawCard deck 2))
preFlopInit :: Int -> IO ([[Card]], [Card])
preFlopInit n = do
  g <- getStdGen
  let shuffled_deck = shuffle g [Card x y | x <- [0..3], y <- [1..13]]
  let (hands, deck_after_preflop) = preflopDrawNplayer n ([], shuffled_deck)
  return (hands, deck_after_preflop)
main :: IO ()
main = do
    let all_suits = ["D", "H", "S", "C"]
    let all_numbers = ["0", "A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]
    let all_hand = ["NoHand", "OnePair", "TwoPair", "ThreeCard", "Straight", "Flash", "FullHouse", "FourCard", "StraightFlash", "RoyalStraightFlash"]
    (hands, deck_after_preflop) <- preFlopInit 4
    let my_inithand = head hands
    let com1_inithand = hands !! 1
    let com2_inithand = hands !! 2
    let com3_inithand = hands !! 3
    let (com_card, deck_after_flop) = drawCard deck_after_preflop 3
    let pre_bet = [50, 25, 0, 0]
    print "pre flop"
    putStrLn ("you: " ++ show(head pre_bet) ++ " com1: " ++ show(pre_bet !! 1) ++ " com2: " ++ show(pre_bet !! 2) ++ " com3: " ++ show(pre_bet !! 3))
    putStrLn "your initial hand is"
    print (convertHandsToShow all_suits all_numbers my_inithand)
    pre_bet_list <- betAction pre_bet
    let after_my_pre_bet = if head pre_bet_list == -1 then replace pre_bet (0, 0) else replace pre_bet (0, head pre_bet_list)
    let after_com1_pre_bet = if head pre_bet_list == -1 then after_my_pre_bet else replace after_my_pre_bet (1, head after_my_pre_bet) 
    let after_com2_pre_bet = if head pre_bet_list == -1 then replace after_com1_pre_bet (2, after_my_pre_bet !! 1) else replace after_com1_pre_bet (2, head after_my_pre_bet)
    let after_com3_pre_bet = if head pre_bet_list == -1 then replace after_com2_pre_bet (3, after_my_pre_bet !! 1) else replace after_com2_pre_bet (3, head after_my_pre_bet)
    let pre_flop_pod = sum after_com3_pre_bet
    putStrLn ("you: " ++ show(head after_com3_pre_bet) ++ " com1: " ++ show(after_com3_pre_bet !! 1) ++ " com2: " ++ show(after_com3_pre_bet !! 2) ++ " com3: " ++ show(after_com3_pre_bet !! 3))
    putStrLn ("pod: " ++ show pre_flop_pod)
    putStrLn ("community card: " ++ show (convertHandsToShow all_suits all_numbers com_card))
    let flop_bet = if head pre_bet_list == -1 then replace [0, 0, 0, 0] (0, -1) else [0, 0, 0, 0]
    print "flop"
    flop_bet_list <- betAction flop_bet
    let after_my_flop_bet = if head flop_bet_list == -1 then replace flop_bet (0, 0) else replace flop_bet (0, head flop_bet_list)
    let after_com1_flop_bet = replace after_my_flop_bet (1, head after_my_flop_bet)
    let after_com2_flop_bet = replace after_com1_flop_bet (2, head after_my_flop_bet)
    let after_com3_flop_bet = replace after_com2_flop_bet (3, head after_my_flop_bet)
    putStrLn ("you: " ++ show(head after_com3_flop_bet) ++ " com1: " ++ show(after_com3_flop_bet !! 1) ++ " com2: " ++ show(after_com3_flop_bet !! 2) ++ " com3: " ++ show(after_com3_flop_bet !! 3))
    let flop_pod = pre_flop_pod + sum after_com3_flop_bet
    putStrLn ("pod: " ++ show flop_pod)
    let com_card_after_turn = com_card ++ [head deck_after_flop]
    let deck_after_turn = tail deck_after_flop
    putStrLn "community card is"
    print (convertHandsToShow all_suits all_numbers com_card_after_turn)
    putStrLn "your hand is"
    print (convertHandsToShow all_suits all_numbers my_inithand)
    print "river"
    let river_bet = if head flop_bet_list == -1 || head after_com1_pre_bet == -1 then replace [0, 0, 0, 0] (0, -1) else [0, 0, 0, 0]
    river_bet_list <- betAction river_bet
    let after_my_river_bet = if head river_bet_list == -1 then replace river_bet (0, 0) else replace river_bet (0, head river_bet_list)
    let after_com1_river_bet = replace after_my_river_bet (1, head after_my_river_bet)
    let after_com2_river_bet = replace after_com1_river_bet (2, head after_my_river_bet)
    let after_com3_river_bet = replace after_com2_river_bet (3, head after_my_river_bet)
    let river_pod = flop_pod + sum after_com3_river_bet
    putStrLn ("you: " ++ show(head after_com3_river_bet) ++ " com1: " ++ show(after_com3_river_bet !! 1) ++ " com2: " ++ show(after_com3_river_bet !! 2) ++ " com3: " ++ show(after_com3_river_bet !! 3))
    putStrLn ("pod: " ++ show river_pod)
    let com_card_after_river = com_card_after_turn ++ [head deck_after_turn]
    let deck_after_river = tail deck_after_turn
    putStrLn "community card is"
    print (convertHandsToShow all_suits all_numbers com_card_after_river)
    print "show down"
    if head pre_bet_list /= -1 && head flop_bet_list /= -1 && head river_bet_list /= -1 then do
      putStrLn "your hand is"
      print (convertHandsToShow all_suits all_numbers my_inithand)
    else do
      putStrLn "you folded"
    putStrLn "com1 hand is"
    print (convertHandsToShow all_suits all_numbers com1_inithand)
    putStrLn "com2 hand is"
    print (convertHandsToShow all_suits all_numbers com2_inithand)
    putStrLn "com3 hand is"
    print (convertHandsToShow all_suits all_numbers com3_inithand)
    let suits_list = [0, 0, 0, 0]
    let numbers_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    let my_counted_suits = countSuits suits_list $ my_inithand ++ com_card_after_river
    let my_counted_numbers = countNumber numbers_list $ my_inithand ++ com_card_after_river
    let com1_counted_suits = countSuits suits_list $ com1_inithand ++ com_card_after_river
    let com1_counted_numbers = countNumber numbers_list $ com1_inithand ++ com_card_after_river
    let com2_counted_suits = countSuits suits_list $ com2_inithand ++ com_card_after_river
    let com2_counted_numbers = countNumber numbers_list $ com2_inithand ++ com_card_after_river
    let com3_counted_suits = countSuits suits_list $ com3_inithand ++ com_card_after_river
    let com3_counted_numbers = countNumber numbers_list $ com3_inithand ++ com_card_after_river
    let my_judge_result = judge my_counted_suits my_counted_numbers
    let com1_judge_result = judge com1_counted_suits com1_counted_numbers
    let com2_judge_result = judge com2_counted_suits com2_counted_numbers
    let com3_judge_result = judge com3_counted_suits com3_counted_numbers
    print ((if head river_bet_list /= -1 then "you: " ++ all_hand !! my_judge_result else "you folded") ++ " com1: " ++ all_hand !! com1_judge_result ++ " com2: " ++ all_hand !! com2_judge_result ++ " com3: " ++ all_hand !! com3_judge_result)
    -- let player_list = ["you", "com1", "com2", "com3"]
    -- let hand_list = [my_judge_result, com1_judge_result, com2_judge_result, com3_judge_result]