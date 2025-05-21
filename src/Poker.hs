module Poker where

import System.Random 
import Data.List (nub, sortOn, sortBy, transpose, maximumBy)
import Data.Ord (comparing)
import Utils
import Control.Monad (forM_)

data Card = Card { mark :: Int, number :: Int } deriving (Eq, Ord, Show)
getNubRandomRs :: RandomGen g => g -> (Int, Int) -> [Int]
getNubRandomRs g (l, h) = take (abs (h - l) + 1) $ nub $ randomRs (l, h) g

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle _ [] = []
shuffle g xs = fmap fst $ sortOn snd $ zip xs rs
    where
        rs = getNubRandomRs g (1, length xs)

playerInit :: Int -> IO [Int]
playerInit n = do
  g <- getStdGen
  let players_list = shuffle g [x | x <- [0..n-1]]
  return players_list

convertCardToShow ::  [String] -> [String] -> Card -> String
convertCardToShow suit_list number_list x = suit_list !! mark x ++ (number_list !! (number x - 1))

convertHandsToShow :: [String] -> [String] -> [Card] -> [String]
convertHandsToShow suit number hand = map (convertCardToShow suit number) hand

countSuits :: [Int] -> [Card] -> [Int]
countSuits xs [] = xs
countSuits xs (x:hand) = countSuits (replace xs (mark x, (xs !! mark x)+1)) hand

countNumber :: [Int] -> [Card] -> [Int]
countNumber xs [] = xs
countNumber xs (x:hand) = countNumber (replace xs ((number x -1), (xs !! (number x - 1))+1)) hand

drawCard :: [Card] -> Int -> ([Card], [Card])
drawCard deck n = (take n deck, removeFromList n deck)

preflopDrawNplayer :: Int -> ([[Card]], [Card]) -> ([[Card]], [Card])
preflopDrawNplayer 0 (hands, deck) = (hands, deck)
preflopDrawNplayer n (hands, deck) = preflopDrawNplayer (n-1) (hands ++ [fst (drawCard deck 2)], snd (drawCard deck 2))

preFlopInit :: RandomGen g => g -> Int -> IO ([[Card]], [Card])
preFlopInit g n = do
  let shuffled_deck = shuffle g [Card x y | x <- [0..3], y <- [2..14]]
  let (hands, deck_after_preflop) = preflopDrawNplayer n ([], shuffled_deck)
  return (hands, deck_after_preflop)

isOnePair :: [Int] -> Bool
isOnePair numbers = length (filter (==2) numbers) == 1

isTwoPair :: [Int] -> Bool
isTwoPair numbers = length (filter (==2) numbers) >= 2 

isThreeCard :: [Int] -> Bool
isThreeCard numbers =  notElem 2 numbers && length (filter (==3) numbers) == 1

isFourCard :: [Int] -> Bool
isFourCard numbers = length (filter (==4) numbers) == 1

isFullHouse :: [Int] -> Bool
isFullHouse numbers = length (filter (==2) numbers) == 1 && length (filter (==3) numbers) == 1

isFlash :: [Int] -> Bool
isFlash suits = length (filter (>=5) suits) == 1

isStraight :: [Int] -> (Bool, Int)
isStraight numbers
  | length (filter (>= 1) (take 5 $ reverse numbers_)) == 5
  = (True, length numbers_)
  | length numbers_ < 5 = (False, 0)
  | otherwise = isStraight $ init numbers_
  where
    numbers_ = if length numbers == 14 && numbers !! 13 >= 1 then replace numbers (0, numbers !! 13) else numbers

isStraightFlash :: [Int] -> [Int] -> Bool
isStraightFlash suits numbers= isFlash suits && fst (isStraight numbers)

isRoyalStraightFlush :: [Int] -> [Int] -> Bool
isRoyalStraightFlush suits numbers = isFlash suits && numbers !! 10 == 1 && numbers !! 11 == 1 && numbers !! 12 == 1 && numbers !! 13 == 1 && numbers !! 1 == 1

getRankOfHand :: [Int] -> [Int] -> [Card] -> [Int]
{-
  return (rank of hand, detail of rank)
-}
getRankOfHand suits numbers cards
  | isRoyalStraightFlush suits numbers = [9, 0]
  | isStraightFlash suits numbers = 8 : [snd $ isStraight numbers]
  | isFourCard numbers = 7: map (+1) (head (findListIndex (== 4) numbers) : [last $ findListIndex ((>=1) .&&. (< 4)) numbers])
  | isFullHouse numbers = 6: map (+1) ([last $ findListIndex (==3) numbers, last $ findListIndex (==2) numbers])
  | isFlash suits = 5: take 5 (sortBy (flip compare) $ map number (filter (\card -> mark card == head (findListIndex (>=5) suits)) cards))
  | fst $ isStraight numbers = 4: [snd $ isStraight numbers]
  | isThreeCard numbers = 3: map (+1) (last (findListIndex (==3) numbers) : take 2 (reverse $ findListIndex (==1) numbers))
  | isTwoPair numbers = 2: map (+1) (take 2 (reverse $ findListIndex (==2) numbers) ++ [if length (findListIndex (==2) numbers) >= 3 && head (findListIndex (==2) numbers) > head (findListIndex (==1) numbers) then head (findListIndex (==2) numbers) else head $ findListIndex (==1) numbers])
  | isOnePair numbers = 1: map (+1) (head (findListIndex (==2) numbers) : take 4 (reverse $ findListIndex (==1) numbers))
  | otherwise = 0: map (+1) (take 5 $ reverse $ findListIndex (==1) numbers)

judgeWinner :: [[Int]] -> Int
judgeWinner ranklist = go (zip [0..] ranklist)
  where
    go :: [(Int, [Int])] -> Int
    go xs
      | null xs || any (null . snd) xs = -1 
      | length xs == 1 = fst $ head xs
      | otherwise =
          let firstElems = map (\(_, x) -> head x) xs
              maxFirstElem = maximum firstElems
              maxElems = filter (\(_, x) -> head x == maxFirstElem) xs
          in if length maxElems == 1
             then fst $ head maxElems
             else go $ map (\(i, x) -> (i, tail x)) maxElems

betAction :: [Int] -> [Int] -> IO ([Int], String)
{-
 input :[you, com1, com2, ...]
-}
betAction pre_bet_amount_list players_list = do
  let current_bet = pre_bet_amount_list !! head (findListIndex (== 0) players_list)
      your_index = head (findListIndex (== 0) players_list)
  if current_bet == -1
    then return (pre_bet_amount_list, "fold")
  else do
    print "select action (-1: fold, 0: check, 1: call, 2: raise)"
    action_string <- getLine
    let action_int = stringToInt action_string
    if action_int == -1 then do
      return (replace pre_bet_amount_list (your_index, -1), "fold")
    else if action_int == 0 then do
      if current_bet /= maximum pre_bet_amount_list then do
        print "you can't check. select other action"
        betAction pre_bet_amount_list players_list
      else do
        return (pre_bet_amount_list, "check")
    else if action_int == 1 then do
      if current_bet == maximum pre_bet_amount_list then do 
        print "you can't call. select other action"
        betAction pre_bet_amount_list players_list
      else do
        return (replace pre_bet_amount_list (your_index, maximum pre_bet_amount_list), "call")
    else if action_int == 2 then do
      print "enter bet amount"
      bet_amount <- getLine
      let bet_amount_int = stringToInt bet_amount
      if bet_amount_int <= 0 then do
        print "invalid input"
        betAction pre_bet_amount_list players_list
      else do
        return (replace pre_bet_amount_list (your_index, current_bet + bet_amount_int), "raise")
    else do
      print "invalid input."
      betAction pre_bet_amount_list players_list

comNActionHandler :: Int -> [Int] -> (String, Int)
{-
  とりあえずcallとcheckしてくるだけ
  i: index
-}
comNActionHandler i bet_amount_list = (if bet_amount_list !! i == maximum bet_amount_list then "check" else "call", maximum bet_amount_list - bet_amount_list !! i)

comBetAction :: Int -> [Int] -> [Int] -> IO ([Int], String)
{-
  i: index (i: Int, com_bet_amount: Int, pre_bet_amount_list: [Int], players_list)
-}
comBetAction i pre_bet_amount_list players_list = do
  let com_bet_amount = snd $ comNActionHandler i pre_bet_amount_list
      computer_number = players_list !! i
  if com_bet_amount /= 0 && com_bet_amount + pre_bet_amount_list !! i == maximum pre_bet_amount_list then do
    putStrLn ("com" ++ show computer_number ++ " called")
    let (before, _:after) = splitAt i pre_bet_amount_list
    return (before ++ [pre_bet_amount_list !! i + com_bet_amount] ++ after, "call")
  else if com_bet_amount == 0 then do
    putStrLn ("com" ++ show computer_number ++ " checked")
    let (before, _:after) = splitAt i pre_bet_amount_list
    return (before ++ [pre_bet_amount_list !! i + com_bet_amount] ++ after, "check")
  else if com_bet_amount + pre_bet_amount_list !! i > maximum pre_bet_amount_list then do
    putStrLn ("com" ++ show computer_number ++ " raised")
    let (before, _:after) = splitAt i pre_bet_amount_list
    return (before ++ [pre_bet_amount_list !! i + com_bet_amount] ++ after, "raise")
  else do 
    putStrLn ("com" ++ show computer_number ++ " folded")
    let (before, _:after) = splitAt i pre_bet_amount_list
    return (before ++ [-1] ++ after, "fold")

bettingRound :: [Int] -> Queue Int -> [Int] -> IO [Int]
bettingRound pre_bet_amount_list betting_queue players_list = do
  {-
    initial betting_queue = foldl snoc empty [2, 3, 1, 0]
    raiseしたらその前のプレイヤーまでをqueueに入れて、queueが空になるまでbettingroundを続ける.
  -}
  if not (null $ fst betting_queue) && length (filter (== -1) pre_bet_amount_list) < length pre_bet_amount_list - 1 then do
    if front betting_queue == 0 then do
      (after_my_bet_list, my_action) <- betAction pre_bet_amount_list players_list
      if my_action == "raise" then do
        let (before, _:after) = splitAt (head $ findListIndex (== 0) players_list) players_list 
        let betting_queue_after = foldl snoc (pop betting_queue) before
        bettingRound after_my_bet_list betting_queue_after players_list
      else do
        bettingRound after_my_bet_list (pop betting_queue) players_list
    else do
      let com_number = front betting_queue
      let com_index = head $ findListIndex (== com_number) players_list
      (after_com_bet_list, com_action) <- comBetAction com_index pre_bet_amount_list players_list
      if com_action == "raise" then do
        let (before, _:after) = splitAt com_index players_list
        let betting_queue_after = foldl snoc (pop betting_queue) before
        bettingRound after_com_bet_list betting_queue_after players_list
      -- if com_number == 2 && com_action == "raise" then do
      --   let betting_queue_after = pop betting_queue
      --   bettingRound after_com_bet_list betting_queue_after
      -- else if com_number == 3 && com_action == "raise" then do
      --   let betting_queue_after = snoc (pop betting_queue) 2
      --   bettingRound after_com_bet_list betting_queue_after
      -- else if com_number == 1 && com_action == "raise" then do
      --   let betting_queue_after = foldl snoc (pop betting_queue) [2, 3]
      --   bettingRound after_com_bet_list betting_queue_after
      else do
        bettingRound after_com_bet_list (pop betting_queue) players_list
  else do
    return pre_bet_amount_list

showDown :: [[Card]] -> [Card] -> [[String]] -> [Int] -> [String] -> IO ()
showDown hands com_card converted_hands bet_amount_list players_list_string = do
  let not_folded_index = findListIndex (/= -1) bet_amount_list
  let suits_list = [0, 0, 0, 0]
  let numbers_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let all_hand = ["NoHand", "OnePair", "TwoPair", "ThreeCard", "Straight", "Flash", "FullHouse", "FourCard", "StraightFlash", "RoyalStraightFlash"]
  if length not_folded_index == 1 then do
    print ("winner: " ++ players_list_string !! head not_folded_index)
  else do
    let counted_suits_list = map (countSuits suits_list . (++ com_card)) hands
    let counted_numbers_list = map (countNumber numbers_list . (++ com_card)) hands
    let rank_list = zipWith3 getRankOfHand counted_suits_list counted_numbers_list (map (++ com_card) hands)
    forM_ not_folded_index $ \i -> do
      print (players_list_string !! i)
      print (converted_hands !! i)
      print (all_hand !! head (rank_list !! i))
    let not_folded_rank_list = reverse $ foldl (\ xs x -> rank_list !! x : xs) [] not_folded_index
    let not_folded_player = reverse $ foldl (\ xs x -> players_list_string !! x : xs) [] not_folded_index
    let winner = judgeWinner not_folded_rank_list
    print (if winner == -1 then "draw" else "winner: " ++ not_folded_player !! winner)
playGame :: [Int] -> IO ()
playGame players_list = do
  let n = length players_list
  let players_list_string = map (\x -> if x == 0 then "you " else "com" ++ show x) players_list
  let all_suits = ["D", "H", "S", "C"]
  let all_numbers = ["0", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]
  let all_hand = ["NoHand", "OnePair", "TwoPair", "ThreeCard", "Straight", "Flash", "FullHouse", "FourCard", "StraightFlash", "RoyalStraightFlash"]
  g <- newStdGen
  (hands, deck_after_preflop) <- preFlopInit g n
  let my_inithand = hands !! head  (findListIndex (== 0) players_list)
  -- let com1_inithand = hands !! 1
  -- let com2_inithand = hands !! 2
  -- let com3_inithand = hands !! 3
  let (com_card, deck_after_flop) = drawCard deck_after_preflop 3
  let init_list = replicate n 0
  let pre_bet = replace (replace init_list (n-1, 50)) (n-2, 25)
  let betting_queue_init = foldl snoc empty players_list
  print "pre flop"
  mapM_ print (zipWith (\player betamount -> player ++ ": " ++ show betamount) players_list_string pre_bet)
  putStrLn "your initial hand is"
  print (convertHandsToShow all_suits all_numbers my_inithand)
  after_preflop_bet_list <- bettingRound pre_bet betting_queue_init players_list
  -- let after_preflop_bet_list2 = if head after_preflop_bet_list == -1 then replace after_preflop_bet_list (0, 0) else after_preflop_bet_list
  -- print after_preflop_bet_list2
  let pre_flop_pod = sum $ filter (>= 0) after_preflop_bet_list
  mapM_ print (zipWith (\player betamount -> player ++ ": " ++ (if betamount /= -1 then show betamount else "folded")) players_list_string after_preflop_bet_list)
  putStrLn ("pod: " ++ show pre_flop_pod)
  print "community card is"
  print (convertHandsToShow all_suits all_numbers com_card)
  putStrLn "your hand is"
  print (convertHandsToShow all_suits all_numbers my_inithand)
  let flop_bet_ = foldl (\xs x-> replace xs (x, -1)) (replicate n 0) (findListIndex (== -1) after_preflop_bet_list)
      flop_bet = last (init flop_bet_) : last flop_bet_ : init (init flop_bet_)
  -- let flop_bet = if head after_preflop_bet_list == -1 then replace [0, 0, 0, 0] (0, -1) else [0, 0, 0, 0]
  print "flop"
  let players_list_after_flop = last (init players_list) : last players_list : init (init players_list)
  let players_list_after_flop_string = map (\x -> if x == 0 then "you " else "com" ++ show x) players_list_after_flop
  let betting_queue_after_flop = foldl snoc empty players_list_after_flop
  after_flop_bet_list <- bettingRound flop_bet betting_queue_after_flop players_list_after_flop
  mapM_ print (zipWith (\player betamount -> player ++ ": " ++ (if betamount /= -1 then show betamount else "folded")) players_list_after_flop_string after_flop_bet_list)
  -- let after_flop_bet_list2 = if head after_flop_bet_list == -1 then replace after_flop_bet_list (0, 0) else after_flop_bet_list
  -- putStrLn ("you: " ++ show(head after_flop_bet_list2) ++ " com1: " ++ show(after_flop_bet_list2 !! 1) ++ " com2: " ++ show(after_flop_bet_list2 !! 2) ++ " com3: " ++ show(after_flop_bet_list2 !! 3))
  let flop_pod =  pre_flop_pod + sum (filter (>= 0) after_flop_bet_list)
  putStrLn ("pod: " ++ show flop_pod)
  let com_card_after_turn = com_card ++ [head deck_after_flop]
  let deck_after_turn = tail deck_after_flop
  putStrLn "community card is"
  print (convertHandsToShow all_suits all_numbers com_card_after_turn)
  putStrLn "your hand is"
  print (convertHandsToShow all_suits all_numbers my_inithand)
  print "river"
  let river_bet = foldl (\xs x-> replace xs (x, -1)) (replicate n 0) (findListIndex (== -1) after_flop_bet_list)
  after_river_bet_list <- bettingRound river_bet betting_queue_after_flop players_list_after_flop
  mapM_ print (zipWith (\player betamount -> player ++ ": " ++ (if betamount /= -1 then show betamount else "folded")) players_list_after_flop_string after_river_bet_list)
  let river_pod = flop_pod + sum (filter (>= 0) after_river_bet_list)
  putStrLn ("pod: " ++ show river_pod)
  let com_card_after_river = com_card_after_turn ++ [head deck_after_turn]
  -- let deck_after_river = tail deck_after_turn
  putStrLn "community card is"
  print (convertHandsToShow all_suits all_numbers com_card_after_river)
  print "show down"
  -- プリフロップ以降順番が変わるので, ハンドのリストも順番を変える--
  let after_flop_hands = last (init hands) : last hands : init (init hands)
  let converted_hands = map (convertHandsToShow all_suits all_numbers) after_flop_hands
  showDown after_flop_hands com_card_after_river converted_hands after_river_bet_list players_list_after_flop_string
  goNext
  where
    goNext = do
        print "go next ? (y/n)"
        next_game_or_quit <- getLine
        if next_game_or_quit == "n" then do
          return ()
        else if next_game_or_quit == "y" then do
          playGame (tail players_list ++ [head players_list])
        else do
          print "invalid input"
          goNext
