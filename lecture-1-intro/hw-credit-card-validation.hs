
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (div n 10) ++ [(mod n 10)]


toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits
{-
toDigitsRev n
  | n <= 0 = []
  | otherwise = (mod n 10) : toDigitsRev (div n 10)
-}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle [1,2])

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev = reverse . doubleEveryOther . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate = ((==) 0) . (flip mod 10) . (sumDigits . doubleEveryOtherRev . toDigits)
-- validate x = (sumDigits . doubleEveryOtherRev . toDigits) x `mod` 10 == 0

