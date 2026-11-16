main :: IO ()
main = do
  print 1
  print 2
  print 3
  print (f 10)
  print (mymin 10 2)
  print $ mymin 10 2
  print $ isInside 4 1 10
  print (myfunc 3 4)
  print $ myfib 5
  print $ fib 5
  print $ fib 500

a = 20
f x = x * 2

mymin a b =
  if a < b then a else b

isInside x a b = a <= x && x <= b

myfunc a b =
  average (sqr a) (sqr b)
  where sqr a = a * a
        average a b = (a + b) / 2

f2 a b c
  | a < 10 = 2
  | b > 10 = 5
  | otherwise = 10

myfib n =
  if n == 0 then 0
  else if n == 1 then 1
  else myfib (n - 2) + myfib (n - 1)

myfib' n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = myfib' (n - 2) + myfib' (n - 1)

myfib'' 0 = 0
myfib'' 1 = 1
myfib'' n = myfib'' (n - 2) + myfib'' (n - 1)

fib 0 = 0
fib n = helper 1 0 1
  where
    helper i prev cur =
      if i == n then cur
      else helper (i + 1) cur (prev + cur)

-- div, mod
mygcd a 0 = a
mygcd a b = mygcd b (a `mod` b)

mymaxdivisor x = helper (x - 1)
  where
    helper d =
      if x `mod` d == 0
        then d
        else helper (d - 1)

mymaxdivisor' x =
  head [d | d <- [x-1,x-2..], x `mod` d == 0]

sumOfOdds a b
  | a > b     = 0
  | odd a     = a + sumOfOdds (a + 1) b
  | otherwise = sumOfOdds (a + 1) b

prime 1 = False
prime n = helper 2
  where
    helper d
      | d == n         = True
      | n `mod` d == 0 = False
      | otherwise      = helper (d + 1)

prime' n = [1, n] == [d | d <- [1..n], n `mod ` d == 0]

reverseNumber n =
  helper 0 n
  where
    helper res 0 = res
    helper res k = helper (res * 10 + mod k 10) (div k 10)

isPalindrome n = n == reverseNumber n

countPalindromes a b
  | a > b          = 0
  | isPalindrome a = 1 + countPalindromes (a + 1) b
  | otherwise      = countPalindromes (a + 1) b

countDivisors n = helper 1
  where
    helper d
      | d > n = 0
      | mod n d == 0 = 1 + helper (d + 1)
      | otherwise = helper (d + 1)
