import Data.List (nub, sortOn, sortBy, transpose, maximumBy)
import Data.Ord (comparing)

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = f a && g a
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
judge :: [Int] -> [Int] -> [Card] -> [Int]
judge suits numbers cards
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
findListIndex :: (a -> Bool) -> [a] -> [Int]
findListIndex predicate xs = map (fst) (filter (predicate . snd) (zip [0..] xs))
data Card = Card { mark :: Int, number :: Int } deriving (Eq, Ord, Show)
countSuits :: [Int] -> [Card] -> [Int]
countSuits xs [] = xs
countSuits xs (x:hand) = countSuits (replace xs (mark x, (xs !! mark x)+1)) hand
countNumber :: [Int] -> [Card] -> [Int]
countNumber xs [] = xs
countNumber xs (x:hand) = countNumber (replace xs ((number x -1), (xs !! (number x - 1))+1)) hand
replace :: [a] -> (Int, a) -> [a]
replace xs (i, e) = before ++ [e] ++ after
  where
    (before, _:after) = splitAt i xs
convertCardToShow ::  [String] -> [String] -> Card -> String
convertCardToShow suit_list number_list x = suit_list !! mark x ++ (number_list !! (number x - 1))
convertHandsToShow :: [String] -> [String] -> [Card] -> [String]
convertHandsToShow suit number hand = map (convertCardToShow suit number) hand
handTest :: [Int] -> [Int] -> [Card] -> [Int]
removeFromList :: Int -> [a] -> [a]
removeFromList _ [] = []
removeFromList 0 xs = xs
removeFromList n (_:xs) = removeFromList (n-1) xs
handTest suits_list numbers_list hand = judge (countSuits suits_list hand) (countNumber numbers_list hand) hand
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

main :: IO ()
main = do
  let all_suits = ["D", "H", "S", "C"]
  let all_numbers = ["0", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]
  let no_pair = [Card 0 14, Card 1 2, Card 3 11, Card 2 3, Card 2 4, Card 3 6, Card 0 7]
  let flash_cards = [Card 0 2, Card 0 3, Card 0 4, Card 0 5, Card 2 4, Card 0 12, Card 0 11]
  let one_pair = [Card 0 14, Card 1 14, Card 3 11, Card 2 3, Card 2 4, Card 3 6, Card 0 7]
  let two_pair = [Card 0 2, Card 1 2, Card 3 3, Card 2 3, Card 2 7, Card 3 8, Card 0 7]
  let three_card = [Card 0 14, Card 1 14, Card 2 14, Card 2 11, Card 2 4, Card 3 6, Card 0 7]
  let straight = [Card 0 2, Card 1 3, Card 2 4, Card 3 5, Card 2 6, Card 3 10, Card 0 14]
  let flash = [Card 0 2, Card 0 5, Card 0 11, Card 0 14, Card 0 6, Card 3 7, Card 1 12]
  let full_house = [Card 0 14, Card 1 14, Card 2 14, Card 2 11, Card 3 11, Card 3 6, Card 0 7]
  let four_card = [Card 0 14, Card 1 14, Card 2 14, Card 2 11, Card 3 11, Card 3 14, Card 0 11]
  let straight_flash = [Card 0 2, Card 0 3, Card 0 4, Card 0 5, Card 0 6, Card 0 7, Card 0 12]
  let suits_list = [0, 0, 0, 0]
  let numbers_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  print (countNumber numbers_list four_card)
  let numbers = convertHandsToShow all_suits all_numbers straight
  print numbers
  let test = handTest suits_list numbers_list straight
  print test
  print (judgeWinner[[0,11,10,10,6,5],[0,14,12,11,10,9],[1,11,10,10,7,6], [1,11,10,10,9,6]])
  print (judgeWinner [[2, 2, 6], [2, 2, 6], [2, 2, 6]])