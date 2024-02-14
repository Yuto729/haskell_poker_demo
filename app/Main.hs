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

main :: IO ()
main = do
    let cards = [Card x y | x <- [0..3], y <- [1..13]]
    let all_suits = ["D", "H", "S", "C"]
    let all_numbers = ["0", "A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]
    let all_hand = ["NoHand", "OnePair", "TwoPair", "ThreeCard", "Straight", "Flash", "FullHouse", "FourCard", "StraightFlash", "RoyalStraightFlash"]
    g <- getStdGen
    let shuffled_cards = shuffle g cards
    let inithand = take 2 shuffled_cards
    let shuffled_cards2 = removeFromList 2 shuffled_cards
    let com1_inithand = take 2 shuffled_cards2
    let shuffled_cards3 = removeFromList 2 shuffled_cards2
    let com2_inithand = take 2 shuffled_cards3
    let shuffled_cards4 = removeFromList 2 shuffled_cards3
    let com3_inithand = take 2 shuffled_cards4
    let shuffled_cards_after_preflop= removeFromList 2 shuffled_cards4
    let com_card = take 3 shuffled_cards_after_preflop
    let shuffled_cards_after_flop = removeFromList 3 shuffled_cards_after_preflop
    let pre_bet = [50, 25, 0, 0]
    putStrLn ("your: " ++ show(head pre_bet) ++ " com1: " ++ show(pre_bet !! 1) ++ " com2: " ++ show(pre_bet !! 2) ++ " com3: " ++ show(pre_bet !! 3))
    putStrLn "your initial hand is"
    print (convertHandsToShow all_suits all_numbers inithand)
    putStrLn "enter bet amount"
    pre_bet_string <- getLine
    let pre_my_bet_amount = stringToInt pre_bet_string
    let after_my_pre_bet = replace pre_bet (0, head pre_bet + pre_my_bet_amount) 
    let after_com1_pre_bet = replace after_my_pre_bet (1, head after_my_pre_bet)
    let after_com2_pre_bet = replace after_com1_pre_bet (2, head after_my_pre_bet)
    let after_com3_prebet = replace after_com2_pre_bet (3, head after_my_pre_bet)
    let pre_flop_pod = sum after_com3_prebet
    putStrLn ("pod: " ++ show pre_flop_pod)
    putStrLn ("community card: " ++ show (convertHandsToShow all_suits all_numbers com_card))
    putStrLn "enter bet amount"
    flop_bet_string <- getLine
    let flop_bet = [0, 0, 0, 0]
    let my_flop_bet_amount = stringToInt flop_bet_string
    let after_my_flop_bet = replace flop_bet (0, my_flop_bet_amount)
    putStrLn ("your: " ++ show(head after_my_flop_bet) ++ " com1: " ++ show(after_my_flop_bet !! 1) ++ " com2: " ++ show(after_my_flop_bet !! 2) ++ " com3: " ++ show(after_my_flop_bet !! 3))
    let flop_pod = pre_flop_pod + sum after_my_flop_bet
    putStrLn ("pod: " ++ show flop_pod)
    let com_card_after_turn = com_card ++ [head shuffled_cards_after_flop]
    let shuffled_cards_after_turn = tail shuffled_cards_after_flop
    putStrLn "community card is"
    print (convertHandsToShow all_suits all_numbers com_card_after_turn)
    putStrLn "your hand is"
    print (convertHandsToShow all_suits all_numbers inithand)
    putStrLn "enter bet amount"
    river_bet_string <- getLine
    let river_bet = [0, 0, 0, 0]
    let com_card_after_river = com_card_after_turn ++ [head shuffled_cards_after_turn]
    putStrLn "community card is"
    print (convertHandsToShow all_suits all_numbers com_card_after_river)
    putStrLn "your hand is"
    print (convertHandsToShow all_suits all_numbers inithand)
    putStrLn "com1 hand is"
    print (convertHandsToShow all_suits all_numbers com1_inithand)
    putStrLn "com2 hand is"
    print (convertHandsToShow all_suits all_numbers com2_inithand)
    putStrLn "com3 hand is"
    print (convertHandsToShow all_suits all_numbers com3_inithand)
    -- let shuffled_cards_after_river = tail shuffled_cards_after_turn
    let suits_list = [0, 0, 0, 0]
    let numbers_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    let my_counted_suits = countSuits suits_list $ inithand ++ com_card_after_river
    let my_counted_numbers = countNumber numbers_list $ inithand ++ com_card_after_river
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
    print ("you: " ++ all_hand !! my_judge_result ++ " com1: " ++ all_hand !! com1_judge_result ++ " com2: " ++ all_hand !! com2_judge_result ++ " com3: " ++ all_hand !! com3_judge_result)
    -- let player_list = ["you", "com1", "com2", "com3"]
    -- let hand_list = [my_judge_result, com1_judge_result, com2_judge_result, com3_judge_result]
