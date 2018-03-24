
module Golf where

import Data.List

{- Exercise 1 -}

skips :: [a] -> [[a]]
skips xs = [ extractNth xs n | n <- [1..length xs] ]

extractNth :: [a] -> Int -> [a]
extractNth [] _ = []
extractNth l n = ((map snd) . (filter ((==) n . fst)) . (zip (cycle [1..n]))) l

{- Exercise 2 -}

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | y > x && y > z = y : localMaxima (y:z:xs)
  | otherwise = localMaxima (y:z:xs)
localMaxima _ = []

{- Exercise 3 -}

histogram :: [Integer] -> String
histogram = (flip (++) rule) . unlines . reverse . printGraph . (tally [0..9])
  where rule = "==========\n0123456789\n"

printGraph :: [Int] -> [String]
printGraph l = [ printCount l n | n <- [0..(maximum l - 1)] ]

printCount :: [Int] -> Int -> String
printCount [] _ = ""
printCount (v:xs) n = c ++ (printCount xs n)
  where c = if (v - n) > 0 then "*" else " "

tally :: [Integer] -> [Integer] -> [Int]
tally p = ((map (\x -> (length x -1))) . group . sort . ((++) p))

