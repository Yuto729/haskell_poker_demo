module Main where
import System.Random
import Poker
import Utils
main :: IO ()
main = do
  -- let players_list = [2, 3, 1, 0] :: [Int] -- your_id: 0. --
  -- let players_list_string = ["you", "com1", "com2", "com3"]
  -- let all_suits = ["D", "H", "S", "C"]
  -- let all_numbers = ["0", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]
  -- let all_hand = ["NoHand", "OnePair", "TwoPair", "ThreeCard", "Straight", "Flash", "FullHouse", "FourCard", "StraightFlash", "RoyalStraightFlash"]
  -- (hands, deck_after_preflop) <- preFlopInit 4
  -- let my_inithand = head hands
  -- let com1_inithand = hands !! 1
  -- let com2_inithand = hands !! 2
  -- let com3_inithand = hands !! 3
  -- let (com_card, deck_after_flop) = drawCard deck_after_preflop 3
  -- let pre_bet = [50, 25, 0, 0]
  -- let betting_queue_init = foldl snoc empty [2, 3, 1, 0]
  -- print "pre flop"
  -- putStrLn ("you: " ++ show(head pre_bet) ++ " com1: " ++ show(pre_bet !! 1) ++ " com2: " ++ show(pre_bet !! 2) ++ " com3: " ++ show(pre_bet !! 3))
  -- putStrLn "your initial hand is"
  -- print (convertHandsToShow all_suits all_numbers my_inithand)
  -- after_preflop_bet_list <- bettingRound pre_bet betting_queue_init
  -- let after_preflop_bet_list2 = if head after_preflop_bet_list == -1 then replace after_preflop_bet_list (0, 0) else after_preflop_bet_list
  -- print after_preflop_bet_list2
  -- let pre_flop_pod = sum after_preflop_bet_list2
  -- putStrLn ("you: " ++ show(head after_preflop_bet_list2) ++ " com1: " ++ show(after_preflop_bet_list2!! 1) ++ " com2: " ++ show(after_preflop_bet_list2 !! 2) ++ " com3: " ++ show(after_preflop_bet_list2 !! 3))
  -- putStrLn ("pod: " ++ show pre_flop_pod)
  -- putStrLn ("community card: " ++ show (convertHandsToShow all_suits all_numbers com_card))
  -- let flop_bet = if head after_preflop_bet_list == -1 then replace [0, 0, 0, 0] (0, -1) else [0, 0, 0, 0]
  -- print "flop"
  -- after_flop_bet_list <- bettingRound flop_bet betting_queue_init
  -- let after_flop_bet_list2 = if head after_flop_bet_list == -1 then replace after_flop_bet_list (0, 0) else after_flop_bet_list
  -- putStrLn ("you: " ++ show(head after_flop_bet_list2) ++ " com1: " ++ show(after_flop_bet_list2 !! 1) ++ " com2: " ++ show(after_flop_bet_list2 !! 2) ++ " com3: " ++ show(after_flop_bet_list2 !! 3))
  -- let flop_pod = pre_flop_pod + sum after_flop_bet_list2
  -- putStrLn ("pod: " ++ show flop_pod)
  -- let com_card_after_turn = com_card ++ [head deck_after_flop]
  -- let deck_after_turn = tail deck_after_flop
  -- putStrLn "community card is"
  -- print (convertHandsToShow all_suits all_numbers com_card_after_turn)
  -- putStrLn "your hand is"
  -- print (convertHandsToShow all_suits all_numbers my_inithand)
  -- print "river"
  -- let river_bet = if head after_flop_bet_list == -1 || head after_preflop_bet_list == -1 then replace [0, 0, 0, 0] (0, -1) else [0, 0, 0, 0]
  -- after_river_bet_list <- bettingRound river_bet betting_queue_init
  -- let after_river_bet_list2 = if head after_river_bet_list == -1 then replace after_river_bet_list (0, 0) else after_river_bet_list
  -- putStrLn ("you: " ++ show(head after_river_bet_list2) ++ " com1: " ++ show(after_river_bet_list2 !! 1) ++ " com2: " ++ show(after_river_bet_list2 !! 2) ++ " com3: " ++ show(after_river_bet_list2 !! 3))
  -- let river_pod = flop_pod + sum after_river_bet_list2
  -- putStrLn ("pod: " ++ show river_pod)
  -- let com_card_after_river = com_card_after_turn ++ [head deck_after_turn]
  -- -- let deck_after_river = tail deck_after_turn
  -- putStrLn "community card is"
  -- print (convertHandsToShow all_suits all_numbers com_card_after_river)
  -- print "show down"
  -- if head after_river_bet_list /= -1 then do
  --   putStrLn "your hand is"
  --   print (convertHandsToShow all_suits all_numbers my_inithand)
  -- else do
  --   putStrLn "you folded"
  -- putStrLn "com1 hand is"
  -- print (convertHandsToShow all_suits all_numbers com1_inithand)
  -- putStrLn "com2 hand is"
  -- print (convertHandsToShow all_suits all_numbers com2_inithand)
  -- putStrLn "com3 hand is"
  -- print (convertHandsToShow all_suits all_numbers com3_inithand)
  -- let suits_list = [0, 0, 0, 0]
  -- let numbers_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  -- {-
  --   [0, 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K, A]
  -- -}
  -- let my_counted_suits = countSuits suits_list $ my_inithand ++ com_card_after_river
  -- let my_counted_numbers = countNumber numbers_list $ my_inithand ++ com_card_after_river
  -- let com1_counted_suits = countSuits suits_list $ com1_inithand ++ com_card_after_river
  -- let com1_counted_numbers = countNumber numbers_list $ com1_inithand ++ com_card_after_river
  -- let com2_counted_suits = countSuits suits_list $ com2_inithand ++ com_card_after_river
  -- let com2_counted_numbers = countNumber numbers_list $ com2_inithand ++ com_card_after_river
  -- let com3_counted_suits = countSuits suits_list $ com3_inithand ++ com_card_after_river
  -- let com3_counted_numbers = countNumber numbers_list $ com3_inithand ++ com_card_after_river
  -- let my_judge_result = getRankOfHand my_counted_suits my_counted_numbers $ my_inithand ++ com_card_after_river
  -- let com1_judge_result = getRankOfHand com1_counted_suits com1_counted_numbers $ com1_inithand ++ com_card_after_river
  -- let com2_judge_result = getRankOfHand com2_counted_suits com2_counted_numbers $ com2_inithand ++ com_card_after_river
  -- let com3_judge_result = getRankOfHand com3_counted_suits com3_counted_numbers $ com3_inithand ++ com_card_after_river
  -- let winner = players_list_string !! judgeWinner ([my_judge_result] ++ [com1_judge_result] ++ [com2_judge_result] ++ [com3_judge_result])
  -- print ([my_judge_result] ++ [com1_judge_result] ++ [com2_judge_result] ++ [com3_judge_result]) 
  -- print ((if head after_river_bet_list /= -1 then "you: " ++ all_hand !! head my_judge_result  else "you folded") ++ " com1: " ++ all_hand !! head com1_judge_result ++ " com2: " ++ all_hand !! head com2_judge_result ++ " com3: " ++ all_hand !! head com3_judge_result)
  -- print ("winner: " ++ winner)
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