isPalindromic :: Integer -> Bool
isPalindromic n
  | n < 10 = False 
  | otherwise = show n == reverse(show n)

reverseNumber :: Integer -> Integer
reverseNumber n = read(reverse(show n)) :: Integer

addReverseNumber :: Integer -> Integer
addReverseNumber n = n + reverseNumber(n)

isLychrelNumber :: Integer -> Int -> Bool
isLychrelNumber n itr
 | itr==0  = True
 | isPalindromic (addReverseNumber n) = False
 | otherwise = isLychrelNumber (addReverseNumber n) (itr - 1)

extructLychrelNumber :: [Integer] -> [Integer] -> [Integer]
extructLychrelNumber [] _ = []
extructLychrelNumber (x: xs) ps
  | isLychrelNumber x 50 = x : extructLychrelNumber xs ps
  | otherwise = extructLychrelNumber xs ps


main = 
  let
    n = [1..10000]
    a = []
    b = extructLychrelNumber n a
  in
    print (length b)
