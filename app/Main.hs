module Main where

import System.Random 
import Data.List (nub, sortOn, sortBy)
data Card = Card { mark :: Int, number :: Int } deriving (Eq, Ord, Show)

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
{-
  ここで, A -> 14に変換する
-}
countNumber xs (x:hand) = countNumber (replace xs (if number x /= 1 then ((number x)-1, (xs !! (number x)-1)+1) else (13, (xs !! 13)+1))) hand
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

isStraight :: [Int] -> (Bool, [Int])
isStraight numbers
  | length (filter (>= 1) (take 5 $ reverse numbers)) == 5
  = (True, [length numbers])
  | length numbers < 5 = (False, 0)
  | otherwise = isStraight $ init numbers

isStraightFlash :: [Int] -> [Int] -> Bool
isStraightFlash suits numbers= isFlash suits && fst (isStraight numbers)
isRoyalStraightFlush :: [Int] -> [Int] -> Bool
isRoyalStraightFlush suits numbers = isFlash suits && numbers !! 10 == 1 && numbers !! 11 == 1 && numbers !! 12 == 1 && numbers !! 13 == 1 && numbers !! 1 == 1
judge :: [Int] -> [Int] -> [Card] -> (Int, [Int])
judge suits numbers cards
  | isRoyalStraightFlush suits numbers = (9, 0)
  | isStraightFlash suits numbers = (8, snd $ isStraight numbers)
  | isFourCard numbers = (7, head (findListIndex (== 4) numbers) : [last $ findListIndex (== 1) numbers])
  | isFullHouse numbers = (6, [last $ findListIndex (==3) numbers, last $ findListIndex (==2) numbers])
  | isFlash suits = (5, take 5 $ sortBy (flip compare) $ map number (filter (\card -> mark card == head (findListIndex (>=5) suits)) cards))
  | fst $ isStraight numbers = (4, snd $ isStraight numbers)
  | isThreeCard numbers = (3, last (findListIndex (==3) numbers) : take 2 (reverse $ findListIndex (==1) numbers))
  | isTwoPair numbers = (2, take 2 $ reverse $ findListIndex (==2) numbers ++ [last $ findListIndex (==1) numbers])
  | isOnePair numbers = (1, head (findListIndex (==2) numbers) : take 4 (reverse $ findListIndex (==1) numbers))
  | otherwise = (0, take 5 $ reverse $ findListIndex (==1) numbers)
betAction :: [Int] -> IO ([Int], String)
{-
 input :[you, com1, com2, ...]
-}
betAction pre_bet_amount_list = do
  if head pre_bet_amount_list == -1
    then return (pre_bet_amount_list, "fold")
  else do
    print "select action (-1: fold, 0: check, 1: call, 2: raise)"
    action_string <- getLine
    let action_int = stringToInt action_string
    if action_int == -1 then do
      return (-1: tail pre_bet_amount_list, "fold")
    else if action_int == 0 then do
      if head pre_bet_amount_list /= maximum pre_bet_amount_list then do
        print "you can't check. select other action"
        betAction pre_bet_amount_list
      else do
        return (pre_bet_amount_list, "check")
    else if action_int == 1 then do
      if head pre_bet_amount_list == maximum pre_bet_amount_list then do 
        print "you can't call. select other action"
        betAction pre_bet_amount_list
      else do
        return (maximum pre_bet_amount_list: tail pre_bet_amount_list, "call")
    else if action_int == 2 then do
      print "enter bet amount"
      bet_amount <- getLine
      let bet_amount_int = stringToInt bet_amount
      if bet_amount_int <= 0 then do
        print "invalid input"
        betAction pre_bet_amount_list
      else do
        return ((head pre_bet_amount_list + bet_amount_int): tail pre_bet_amount_list,"raise")
    else do
      print "invalid input."
      betAction pre_bet_amount_list
comNActionHandler :: Int -> [Int] -> (String, Int)
{-
  とりあえずcallとcheckしてくるだけ
-}
comNActionHandler n bet_amount_list = (if bet_amount_list !! n == maximum bet_amount_list then "check" else "call", maximum bet_amount_list - bet_amount_list !! n)

comBetAction :: Int -> [Int] -> IO ([Int], String)
{-
 computer n's action (n: Int, com_bet_amount: Int, pre_bet_amount_list: [Int])
-}
comBetAction n pre_bet_amount_list = do
  let com_bet_amount = snd $ comNActionHandler n pre_bet_amount_list
  if com_bet_amount /= 0 && com_bet_amount + pre_bet_amount_list !! n == maximum pre_bet_amount_list then do
    putStrLn ("com" ++ show n ++ " called")
    let (before, _:after) = splitAt n pre_bet_amount_list
    return (before ++ [pre_bet_amount_list !! n + com_bet_amount] ++ after, "call")
  else if com_bet_amount == 0 then do
    putStrLn ("com" ++ show n ++ " checked")
    let (before, _:after) = splitAt n pre_bet_amount_list
    return (before ++ [pre_bet_amount_list !! n + com_bet_amount] ++ after, "check")
  else if com_bet_amount + pre_bet_amount_list !! n > maximum pre_bet_amount_list then do
    putStrLn ("com" ++ show n ++ " raised")
    let (before, _:after) = splitAt n pre_bet_amount_list
    return (before ++ [pre_bet_amount_list !! n + com_bet_amount] ++ after, "raise")
  else do 
    putStrLn ("com" ++ show n ++ " folded")
    let (before, _:after) = splitAt n pre_bet_amount_list
    return (before ++ [-1] ++ after, "fold")

bettingRound :: [Int] -> Queue Int -> IO [Int]
bettingRound pre_bet_amount_list betting_queue = do
  {-
    initial betting_queue = foldl snoc empty [2, 3, 1, 0]
    raiseしたらその前のプレイヤーまでをqueueに入れて、queueが空になるまでbettingroundを続ける.
  -}
  if not $ null $ fst betting_queue then do
    if front betting_queue == 0 then do
      (after_my_bet_list, my_action) <- betAction pre_bet_amount_list
      if my_action == "raise" then do
        let betting_queue_after = foldl snoc (pop betting_queue) [2, 3, 1]
        bettingRound after_my_bet_list betting_queue_after
      else do
        bettingRound after_my_bet_list (pop betting_queue)
    else do
      let com_number = front betting_queue
      (after_com_bet_list, com_action) <- comBetAction com_number pre_bet_amount_list
      if com_number == 2 && com_action == "raise" then do
        let betting_queue_after = pop betting_queue
        bettingRound after_com_bet_list betting_queue_after
      else if com_number == 3 && com_action == "raise" then do
        let betting_queue_after = snoc (pop betting_queue) 2
        bettingRound after_com_bet_list betting_queue_after
      else if com_number == 1 && com_action == "raise" then do
        let betting_queue_after = foldl snoc (pop betting_queue) [2, 3]
        bettingRound after_com_bet_list betting_queue_after
      else do
        bettingRound after_com_bet_list (pop betting_queue)
  else do
    return pre_bet_amount_list
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
findListIndex :: (a -> Bool) -> [a] -> [Int]
findListIndex predicate xs = map (fst) (filter (predicate . snd) (zip [0..] xs))

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
    let betting_queue_init = foldl snoc empty [2, 3, 1, 0]
    print "pre flop"
    putStrLn ("you: " ++ show(head pre_bet) ++ " com1: " ++ show(pre_bet !! 1) ++ " com2: " ++ show(pre_bet !! 2) ++ " com3: " ++ show(pre_bet !! 3))
    putStrLn "your initial hand is"
    print (convertHandsToShow all_suits all_numbers my_inithand)
    after_preflop_bet_list <- bettingRound pre_bet betting_queue_init
    let after_preflop_bet_list2 = if head after_preflop_bet_list == -1 then replace after_preflop_bet_list (0, 0) else after_preflop_bet_list
    print after_preflop_bet_list2
    let pre_flop_pod = sum after_preflop_bet_list2
    putStrLn ("you: " ++ show(head after_preflop_bet_list2) ++ " com1: " ++ show(after_preflop_bet_list2!! 1) ++ " com2: " ++ show(after_preflop_bet_list2 !! 2) ++ " com3: " ++ show(after_preflop_bet_list2 !! 3))
    putStrLn ("pod: " ++ show pre_flop_pod)
    putStrLn ("community card: " ++ show (convertHandsToShow all_suits all_numbers com_card))
    let flop_bet = if head after_preflop_bet_list == -1 then replace [0, 0, 0, 0] (0, -1) else [0, 0, 0, 0]
    print "flop"
    after_flop_bet_list <- bettingRound flop_bet betting_queue_init
    let after_flop_bet_list2 = if head after_flop_bet_list == -1 then replace after_flop_bet_list (0, 0) else after_flop_bet_list
    putStrLn ("you: " ++ show(head after_flop_bet_list2) ++ " com1: " ++ show(after_flop_bet_list2 !! 1) ++ " com2: " ++ show(after_flop_bet_list2 !! 2) ++ " com3: " ++ show(after_flop_bet_list2 !! 3))
    let flop_pod = pre_flop_pod + sum after_flop_bet_list2
    putStrLn ("pod: " ++ show flop_pod)
    let com_card_after_turn = com_card ++ [head deck_after_flop]
    let deck_after_turn = tail deck_after_flop
    putStrLn "community card is"
    print (convertHandsToShow all_suits all_numbers com_card_after_turn)
    putStrLn "your hand is"
    print (convertHandsToShow all_suits all_numbers my_inithand)
    print "river"
    let river_bet = if head after_flop_bet_list == -1 || head after_preflop_bet_list == -1 then replace [0, 0, 0, 0] (0, -1) else [0, 0, 0, 0]
    after_river_bet_list <- bettingRound river_bet betting_queue_init
    let after_river_bet_list2 = if head after_river_bet_list == -1 then replace after_river_bet_list (0, 0) else after_river_bet_list
    putStrLn ("you: " ++ show(head after_river_bet_list2) ++ " com1: " ++ show(after_river_bet_list2 !! 1) ++ " com2: " ++ show(after_river_bet_list2 !! 2) ++ " com3: " ++ show(after_river_bet_list2 !! 3))
    let river_pod = flop_pod + sum after_river_bet_list2
    putStrLn ("pod: " ++ show river_pod)
    let com_card_after_river = com_card_after_turn ++ [head deck_after_turn]
    let deck_after_river = tail deck_after_turn
    putStrLn "community card is"
    print (convertHandsToShow all_suits all_numbers com_card_after_river)
    print "show down"
    if head after_preflop_bet_list /= -1 && head after_flop_bet_list /= -1 && head after_river_bet_list /= -1 then do
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
    {-
      [0, 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K, A]
    -}
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
    print ((if head after_river_bet_list /= -1 then "you: " ++ all_hand !! my_judge_result else "you folded") ++ " com1: " ++ all_hand !! com1_judge_result ++ " com2: " ++ all_hand !! com2_judge_result ++ " com3: " ++ all_hand !! com3_judge_result)