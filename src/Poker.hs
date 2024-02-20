module Poker where

import System.Random 
import Data.List (nub, sortOn, sortBy, transpose, maximumBy)
import Data.Ord (comparing)
import Utils

data Card = Card { mark :: Int, number :: Int } deriving (Eq, Ord, Show)
getNubRandomRs :: RandomGen g => g -> (Int, Int) -> [Int]
getNubRandomRs g (l, h) = take (abs (h - l) + 1) $ nub $ randomRs (l, h) g

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle _ [] = []
shuffle g xs = fmap fst $ sortOn snd $ zip xs rs
    where
        rs = getNubRandomRs g (1, length xs)

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

preFlopInit :: Int -> IO ([[Card]], [Card])
preFlopInit n = do
  g <- getStdGen
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