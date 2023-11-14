-- CSci 119, Lab 3

-- See https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html
import Data.List (sort, stripPrefix)


---------------- General list functions

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Cartesian product, preserves normalization
cart :: [a] -> [b] -> [(a,b)]
cart [] _ = []
cart _ [] = []
cart xs ys = [(x, y) | x <- xs, y <- ys]
{-
ghci> cart ['a','b'] ['c','d','e']
[('a','c'),('a','d'),('a','e'),('b','c'),('b','d'),('b','e')]

ghci> cart [0,1][2,3]
[(0,2),(0,3),(1,2),(1,3)]

ghci> cart [0,1][2,3,4]
[(0,2),(0,3),(0,4),(1,2),(1,3),(1,4)]
-}

-- Powerset, preserves normalization. Examples:
-- power [] = [[]]
-- power [1] = [[],[1]]
-- power [1,2] = [[],[1],[1,2],[2]]
-- power [1,2,3] = [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = [[]] ++ [[x] ++ ys | ys <- (power xs)] ++ tail (power xs)
{-
ghci> power[]
[[]]

ghci> power [1]
[[],[1]]

ghci> power[1,2]
[[],[1],[1,2],[2]]

ghci> power [1,2,3]
[[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]

ghci> power [0,1,2,3]
[[],[0],[0,1],[0,1,2],[0,1,2,3],[0,1,3],[0,2],[0,2,3],[0,3],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]] 
-}

---------------- Length-ordered lists

-- Length-Ordered Lists over "character type" a (aka "strings over a")
-- Invariant: In LOL n xs, n == length xs
-- Note that automatically-derived Ord instance correctly orders LOLs
data LOL a = LOL Int [a] deriving (Show, Eq, Ord)

-- Alternative Show instance that hides length
-- instance Show a => Show (LOL a) where
--  show (LOL n xs) = show xs
  
-- Empty list (epsilon)
eps :: LOL a
eps = LOL 0 []

{- Here is the example of epsilon which returns empty list
ghci> eps
LOL 0 []
-}

-- Smart constructor for LOL a, establishes invariant
lol :: [a] -> LOL a
lol xs = LOL (length xs) xs

{-
ghci> lol [1..5]    
LOL 5 [1,2,3,4,5]
ghci> lol ['a'..'z']
LOL 26 "abcdefghijklmnopqrstuvwxyz"
-}

-- Concatenation of LOLs, preserves invariant
dot :: LOL a -> LOL a -> LOL a
dot (LOL x xs) (LOL y ys) = LOL (x+y) (xs ++ ys)

{-
ghci> dot (LOL 4 "abcd") (LOL 3 "efg")
LOL 7 "abcdefg"
-}

-- Reverse of LOLs, preserves invariant
rev :: LOL a -> LOL a
rev (LOL x xs) = LOL x (reverse xs)

{-
ghci> rev (LOL 4 "abcd")
LOL 4 "dcba"
-}



---------------- Languages

-- Normalized lists of LOLs (aka "languages")
-- Invariant: xs :: Lang a implies xs is ordered with no duplicates
type Lang a = [LOL a]


-- Constructor for languages, establishes invariant
lang :: Ord a => [[a]] -> Lang a
lang xs = norm $ map lol xs

-- Membership for languages (infinite lists satisfying invariant included)
memb :: Ord a => LOL a -> Lang a -> Bool
memb x [] = False
memb x (y:xs) = case compare x y of
                 LT -> False
                 EQ -> True
                 GT -> memb x xs

{-
ghci> memb (LOL 3 [1,2,3]) (lang [[1..20]])
False
ghci> memb (LOL 3 [1,2,3]) (lang [[1,2,3]])
True
ghci> memb (lol "abc") (lang ["abc","def"])
True
ghci> memb (lol "ab") (lang ["abc"])
False
-}

-- Merge of langages (aka "union")
merge :: Ord a => Lang a -> Lang a -> Lang a
merge [] [] = []
merge xs [] = [] ++ xs
merge [] ys = [] ++ ys
merge (x:xs) (y:ys)
  | x < y = x:(merge xs (y:ys))
  | x > y = y:(merge (x:xs) ys)
  | otherwise = x:(merge xs ys)

{-
ghci> merge (lang ["abc"]) (lang["xyz"])
[LOL 3 "abc",LOL 3 "xyz"]
-}

-- Concatenation of languages
cat :: Ord a => Lang a -> Lang a -> Lang a
cat _ [] = []
cat [] _ = []
cat (x:xs) (y:ys) = (dot x y) : merge ([dot a b | a <- [x], b <- ys]) (cat xs (y:ys))

{-
ghci> cat (lang ["abc"]) (lang ["xyz"])
[LOL 6 "abcxyz"]
-}

-- Kleene star of languages
kstar :: Ord a => Lang a -> Lang a
kstar [] = [eps]
kstar (LOL 0 []:xr) = kstar xr 
kstar xs = eps : cat xs (kstar xs)

{-
ghci> take 5 $ kstar (lang["a"])
[LOL 0 "",LOL 1 "a",LOL 2 "aa",LOL 3 "aaa",LOL 4 "aaaa"]
-}

-- Left quotient of a language by an LOL (cf. Definition 2.16)
-- Hint: Use the stripPrefix function
leftq :: Ord a => LOL a -> Lang a -> Lang a
leftq (LOL n xs) [] = []
leftq (LOL n xs) ((LOL m ys):zs) = case stripPrefix xs ys of
  Nothing -> leftq (LOL n xs) zs
  Just cs -> (LOL (m - n) (cs)):leftq (LOL n xs) (zs)

{-
ghci> leftq (lol "a") (lang ["abcdabcd", "arrow", "aeroplane"])
[LOL 4 "rrow",LOL 7 "bcdabcd",LOL 8 "eroplane"]
ghci> leftq (lol "a") (lang ["language","adequate"])
[LOL 7 "dequate"]
-}

---- Regular expressions and the languages they denote 
data RegExp = Empty                -- Empty language
            | Let Char             -- Single letter language
            | Union RegExp RegExp  -- Union
            | Cat RegExp RegExp    -- Concatenation
            | Star RegExp          -- Kleene star
            deriving (Show, Eq)

-- Compact display form for RegExp
newtype Compact = Compact RegExp

instance (Show Compact) where    -- use precedence to minimize parentheses
  showsPrec d (Compact r) = sp d r where
    sp d Empty         = showString "0"
    sp d (Let c)       = showString [c]
    sp d (Union r1 r2) = showParen (d > 6) $  -- prec(Union) = 6
                         sp 6 r1 .
                         showString "+" .
                         sp 6 r2
    sp d (Cat r1 r2)   = showParen (d > 7) $  -- prec(Cat) = 7
                         sp 7 r1 .
                         sp 7 r2
    sp d (Star Empty)  = showString "1"
    sp d (Star r1)     = sp 9 r1 .     -- prec(Star) = 8
                         showString "*"


-- Quick and dirty postfix RegExp parser, gives non-exaustive match on error
toRE :: String -> RegExp
toRE w = go w [] where
  go [] [r]              = r
  go ('+':xs) (r2:r1:rs) = go xs (Union r1 r2:rs)
  go ('.':xs) (r2:r1:rs) = go xs (Cat r1 r2:rs)
  go ('*':xs) (r:rs)     = go xs (Star r:rs)
  go ('0':xs) rs         = go xs (Empty:rs)
  go ('1':xs) rs         = go xs (Star Empty:rs)
  go (x:xs) rs           = go xs (Let x:rs)


-- The one-string and finite languages of Theorem 3.2. It should be the case
-- that, for any string w, lang_of (onestr w) == [w], and for any (finite) list
-- of (distinct, sorted) strings l, lang_of (finite l) == l.
onestr :: String -> RegExp
onestr [] = Star Empty
onestr [x] = Let x
onestr (x:xs) = Cat (Let x) (onestr xs)
{-
ghci> onestr "abc"
Cat (Let 'a') (Cat (Let 'b') (Let 'c'))
ghci> onestr "abc def ghi"
Cat (Let 'a') (Cat (Let 'b') (Cat (Let 'c') (Cat (Let ' ') (Cat (Let 'd') (Cat (Let 'e') (Cat (Let 'f') (Cat (Let ' ') (Cat (Let 'g') (Cat (Let 'h') (Let 'i'))))))))))
-}

finite :: [String] -> RegExp
finite [] = Empty
finite [x] = onestr x
finite (x:xs) = Union (onestr x) (finite xs)
{-
ghci> finite ["abcd", "efghi"]
Union (Cat (Let 'a') (Cat (Let 'b') (Cat (Let 'c') (Let 'd')))) (Cat (Let 'e') (Cat (Let 'f') (Cat (Let 'g') (Cat (Let 'h') (Let 'i')))))
-}


-- The language associated to a regular expression, i.e., [[r]]
lang_of :: RegExp -> Lang Char
lang_of (Empty) = []
lang_of (Union r1 r2) = merge (lang_of r1) (lang_of r2)
lang_of (Cat r1 r2) = cat (lang_of r1) (lang_of r2)
lang_of (Star r1) = kstar (lang_of r1)
lang_of (Let x) = lang [[x]]

{-
ghci> lang_of (Let 'a')
[LOL 1 "a"]
ghci> lang_of (Cat (Let 'a') (Cat (Let 'b') (Let 'c')))
[LOL 3 "abc"]
ghci> lang_of (Union (Cat (Let 'a') (Cat (Let 'b') (Cat (Let 'c') (Let 'd')))) (Cat (Let 'e') (Cat (Let 'f') (Cat (Let 'g') (Cat (Let 'h') (Let 'i'))))))
[LOL 4 "abcd",LOL 5 "efghi"]
-}


-- Test all of the above operations extensively!            
