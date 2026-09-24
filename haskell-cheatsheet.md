# Haskell - основни блокчета

Вградени форми и функции от втората половина. Файлът е `.hs`.
В GHCi: `ghci file.hs`, презареждане с `:r`, тип на израз с `:t`.

Резултатите по-долу са записани като `израз -- → резултат`.

## Типове, if, пазачи, образци

```haskell
mymin :: Int -> Int -> Int
mymin a b = if a < b then a else b
-- mymin 10 2 -- → 2

abs' n
  | n < 0     = -n
  | otherwise = n
-- abs' (-3) -- → 3

len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs
-- len [1,2,3] -- → 3

sqAvg a b = average (a * a) (b * b)
  where average x y = (x + y) / 2
-- sqAvg 3 4 -- → 12.5
```

`if` иска израз от тип `Bool`. Само `True` и `False` са истина и лъжа.

Пазачите `|` се проверяват отгоре надолу. `otherwise` е последният клон.

Образецът `(x:xs)` разделя списък на глава и опашка. `[]` е празният списък.
`_` е образец, който не кръщава стойността.

`where` слага помощни имена след тялото. Виждат аргументите на функцията.

Скобите и `$` групират едно и също. `$` е със слаб приоритет и маха едни скоби.

```haskell
print (mymin 10 2)
print $ mymin 10 2
```

## Числа

```haskell
10 + 3          -- → 13
10 - 3          -- → 7
2 * 4           -- → 8
10 / 4          -- → 2.5. деление на дробни
div 10 3        -- → 3
mod 10 3        -- → 1
10 `div` 3      -- → 3. инфиксно
10 `mod` 3      -- → 1
abs (-3)        -- → 3
min 4 1         -- → 1
max 4 1         -- → 4
odd 11          -- → True
even 10         -- → True
even 11         -- → False
```

Последната цифра на `n` е `mod n 10`. Без нея е `div n 10`.

`==` сравнява стойности, които са в класа `Eq`. `<`, `<=`, `>`, `>=` искат `Ord`.

## Списъци

```haskell
[]                          -- → []
1 : [2,3]                   -- → [1,2,3]
[1,2] ++ [3,4]              -- → [1,2,3,4]
head [1,2,3]                -- → 1
tail [1,2,3]                -- → [2,3]
null []                     -- → True
null [1]                    -- → False
length [1,2,3]              -- → 3
reverse [1,2,3]             -- → [3,2,1]
take 2 [0,1,2,3]            -- → [0,1]
drop 2 [0,1,2,3]            -- → [2,3]
[1..5]                      -- → [1,2,3,4,5]
[1,3..9]                    -- → [1,3,5,7,9]
zip [1,2] "ab"              -- → [(1,'a'),(2,'b')]
splitAt 3 [1..5]            -- → ([1,2,3],[4,5])
span (< 3) [1,2,4,3]        -- → ([1,2],[4,3])
takeWhile (< 3) [1,2,4]     -- → [1,2]
dropWhile (< 3) [1,2,4]     -- → [4]
elem 2 [1,2,3]              -- → True
delete 2 [1,2,3,2]          -- → [1,3,2]
```

Генератор на списък слага условието след запетая.

```haskell
[x * x | x <- [1,2,3]]            -- → [1,4,9]
[x | x <- [1..10], odd x]         -- → [1,3,5,7,9]
```

Безкраен списък се смята само до поискания префикс.

```haskell
take 5 [1..]                      -- → [1,2,3,4,5]
```

## Функции от по-висок ред

```haskell
map (* 2) [1,2,3]                 -- → [2,4,6]
filter odd [1,2,3,4,5]            -- → [1,3,5]
filter (>= 5) [3,5,7]             -- → [5,7]
foldl (+) 0 [1,2,3]               -- → 6
foldr (:) [] [1,2,3]              -- → [1,2,3]
foldl (flip (:)) [] [1,2,3]       -- → [3,2,1]
sum [1,2,3]                       -- → 6
product [2,3,4]                   -- → 24
and [True, False]                 -- → False
all even [2,4,6]                  -- → True
zipWith (==) "ab" "ac"           -- → [True,False]
maximum [1,3,2]                   -- → 3
minimum [1,3,2]                   -- → 1
```

Частично прилагане и сечение:

```haskell
(* 2) 4                           -- → 8
(`mod` 10) 23                     -- → 3
map (2 *) [1,2,3]                 -- → [2,4,6]
```

Композиция `.` прилага дясната функция първо.

```haskell
(succ . (* 2)) 3                  -- → 7
```

`flip` разменя първите два аргумента.

```haskell
flip (-) 10 3                     -- → -7
```

Сортиране и групиране. `sort`, `sortOn`, `group`, `groupBy` и
`maximumBy` са в `Data.List`.

```haskell
import Data.List
import Data.Function

sort [3,1,2]                              -- → [1,2,3]
sortOn snd [(1,3),(2,1)]                  -- → [(2,1),(1,3)]
group "aabccc"                            -- → ["aa","b","ccc"]
groupBy (\a b -> a == b) [1,1,2,2,1]      -- → [[1,1],[2,2],[1]]
maximumBy (compare `on` length) ["a","bbb","cc"]
                                          -- → "bbb"
```

`group` и `groupBy` лепят само съседни равни елементи. Преди тях
списъкът се сортира, ако групите трябва да съберат всички равни.

## Низове и знакове

`String` е `[Char]`.

```haskell
"ab" ++ "c"                       -- → "abc"
reverse "ab"                      -- → "ba"
show 2                            -- → "2"
read "2" :: Int                   -- → 2
```

`Data.Char`:

```haskell
import Data.Char

ord 'a'                           -- → 97
chr 65                            -- → 'A'
digitToInt '7'                    -- → 7
isDigit '7'                       -- → True
isDigit 'a'                       -- → False
```

Главна латинска буква от малка: `chr (ord c - ord 'a' + ord 'A')`.

## Алгебрични типове

```haskell
data Color = Red | Green | Blue
  deriving (Eq, Show)

Red == Red                        -- → True
show Red                          -- → "Red"

data Shape = Circle Double
           | Rectangle Double Double

area :: Shape -> Double
area (Circle r)      = pi * r * r
area (Rectangle a b) = a * b

data BTree a = Empty | Node a (BTree a) (BTree a)
  deriving Show
```

Името на типа и конструкторите започват с главна буква.
`deriving (Eq, Show)` дава `==` и `show`. Свой `show` се пише с
`instance Show Shape where`.

`type` дава друго име на съществуващ тип. Не прави нов тип.

```haskell
type Pair = (Int, Int)
```

Класът ограничава типа. `Num a` значи, че `a` се събира.
`Eq a` значи, че стойностите се сравняват с `==`.

```haskell
sumTree :: Num a => BTree a -> a
sumTree Empty          = 0
sumTree (Node v lt rt) = v + sumTree lt + sumTree rt
```

## Функтор

`Functor` прилага функция върху стойностите вътре в структурата.
Формата на структурата остава.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

fmap (+1) [1,2,3]                 -- → [2,3,4]
(+1) <$> [1,2,3]                  -- → [2,3,4]. същото, инфиксно
(+1) <$> Just 5                   -- → Just 6
(+1) <$> Nothing                  -- → Nothing
```

`<$>` е инфиксното име на `fmap`.

Двата закона, които инстанцията трябва да спазва:

- `fmap id` е същото като `id`
- `fmap (f . g)` е същото като `fmap f . fmap g`

Компилаторът не проверява законите.

## Вход, изход и Monad

`IO` е структура за стъпка със страничен ефект. `do` ги нарежда.

```haskell
main :: IO ()
main = do
  print 1
  print 2
```

`Maybe` спира веригата при `Nothing`.

```haskell
Just 3 >>= (\x -> Just (x + 1))   -- → Just 4
Nothing >>= (\x -> Just (x + 1))  -- → Nothing
return 3 :: Maybe Int             -- → Just 3
```

`>>=` подава стойността от структурата на следващата функция.
`return` слага стойност в структурата.
