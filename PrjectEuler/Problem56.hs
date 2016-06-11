digitalSum :: Integer -> Integer
digitalSum n
  | n < 10 = n
  | otherwise = accumurateDigits n 0 

accumurateDigits :: Integer -> Integer -> Integer
accumurateDigits n s
  | n < 10 = s + n
  | otherwise = accumurateDigits (n `div` 10) (s + n `mod` 10)

getMaxDigitalSum :: [Integer] -> [Integer] -> Integer -> Integer
getMaxDigitalSum [] _ max = max
getMaxDigitalSum (a : as) [] max = getMaxDigitalSum as [1..99] max
getMaxDigitalSum (a : as) (b : bs) max
  | max < accumurateDigits (a^b) 0 = getMaxDigitalSum (a:as)  bs (accumurateDigits (a^b) 0)
  | otherwise = getMaxDigitalSum (a:as) bs max

main = print (getMaxDigitalSum [1..99] [1..99] 0)
