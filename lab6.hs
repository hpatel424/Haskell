-- Lab 6:  FSM constructions for regular operators

import Data.List
import Data.Array


-- Fixed alphabet, but everything below should work for any sigma!
sigma :: [Char]
sigma = "ab"

-- Recursive reimplementation of strings function from Lab 4
strings :: Int -> [String]
strings n = concat [strs i | i <- [0..n]] where
  strs 0 = [""]
  strs n = [a:xs | a <- sigma, xs <- strs (n-1)]

-- Finite state machines, now indexed by the type of their states
-- M = (states, start, finals, transitions)  
type FSM a = ([a], a, [a], a -> Char -> a)


---------------- Your solution to Lab 5, ported to FSM a -------------------
 -- nodups xs = "xs has no duplicates"
nodups :: Eq a => [a] -> Bool
nodups [] = True           
nodups (x:xs) = notElem x xs && nodups xs

-- subset xs ys == "xs is a subset of ys"
subset :: Eq a => [a] -> [a] -> Bool
subset [] ys = True
subset (x:xs) ys = elem x ys && subset xs ys
-- OR: subset xs ys = all (`elem` ys) xs

-- func d qs == "d is a function from qs x sigma to qs"
func :: Eq a => (a -> Char -> a) -> [a] -> Bool
func d qs = and [elem (d q a) qs | q <- qs, a <- sigma]

-- check whether a finite state machine is correct/complete:
checkFSM :: Eq a => FSM a -> Bool
checkFSM (qs, s, fs, d) = nodups qs &&         -- (1)
                          elem s qs &&         -- (2)
                          subset fs qs &&      -- (3)
                          func d qs            -- (4)
                           
-- All functions below assume that the machine is correct

delta_star :: Eq a => FSM a -> a -> [Char] -> a
delta_star (_, _, _, d) = foldl d

accept1 :: Eq a => FSM a -> [Char] -> Bool
accept1 m@(_, s, fs, _) w = elem (delta_star m s w) fs

accept2_aux :: Eq a => FSM a -> a -> [Char] -> Bool
accept2_aux m@(_, _, fs, _) q [] = elem q fs
accept2_aux m@(_, _, _, d) q (a:w) = accept2_aux m (d q a) w

accept2 :: Eq a => FSM a -> [Char] -> Bool
accept2 m@(_, s, _, _) w = accept2_aux m s w

-- odd_bs is a machine that accepts strings with an odd number of 'b's
-- states: (number of 'b's read so far) mod 2
odd_bs :: FSM Int
odd_bs = ([0,1], 0, [1], d) where
  d q a = if a == 'b' then 1 - q else q

-- avoid w is a machine that accepts strings that don't have w as a substring
-- states: prefixes of w read so far (with w itself as a trap state)
avoid :: String -> FSM String
avoid w = (qs, "", init qs, d) where
  qs = inits w
  d q a = let qa = q ++ [a]
          in if q == w then w else head [xs | xs <- tails qa, isPrefixOf xs w]

-- no_aab is a machine that accepts strings that don't have "aab" as a substring
no_aab :: FSM String
no_aab = avoid "aab"

-- ends_in w is a machine that accepts strings that end in w
-- states: prefixes of w; current state = largest prefix just read
ends_in :: String -> FSM String
ends_in w = (qs, "", [w], d) where
  qs = inits w
  d q a = let qa = q ++ [a]
          in last [xs | xs <- qs, isSuffixOf xs qa]

-- ends_in_ab is a machine that accepts strings ending in "ab"
ends_in_ab :: FSM String
ends_in_ab = ends_in "ab"

-- Define a function that takes a string and returns a machine that accepts
-- exactly that string and nothing else (compare to 'onestr' from Lab 3)
-- Hint: the expression w !! i gives the i-th character of the list/string w
exactly :: String -> FSM Int
exactly w = ([0..n+1], 0, [n], d) where
  n = length w
  d i x = if i < n && w !! i == x then i+1 else n+1
-- Note: in most cases, you will need to add an Eq a => constraint


---------------- Some additional useful functions --------------------------

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Cartesian product (preserves normalization)
(><) :: [a] -> [b] -> [(a,b)]
xs >< ys = [(x,y) | x <- xs, y <- ys]

-- Powerset  (preserves normalization)
power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = let ys = power xs
               in [] : map (x:) ys ++ tail ys

-- Check whether two lists overlap
overlap :: Eq a => [a] -> [a] -> Bool
overlap [] ys = False
overlap (x:xs) ys = elem x ys || overlap xs ys


---------------- Lab 6 begins here -----------------------------------------

-- Complete the FSM constructions for the regular expression operators
-- and test your functions adquately


-- Machine that accepts the empty language
emptyFSM :: FSM Int
emptyFSM = ([0,1], 0, [0], d) where
  d _ _ = 1

-- Machine that accepts the language {"a"} where a in sigma
letterFSM :: Char -> FSM Int
letterFSM a = ([0..2], 0, [1], d) where
  d q b
    | (b == a) && (q == 0) = 1
    | otherwise = 2

-- Machine that accepts the union of the languages accepted by m1 and m2
unionFSM :: (Eq a, Eq b) => FSM a -> FSM b -> FSM (a, b)
unionFSM (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
  qs = qs1 >< qs2
  s  = (s1, s2)
  fs = [(q1, q2) | q1 <- qs1, q2 <- qs2, (elem q1 fs1) || (elem q2 fs2)]
  d (q1, q2) a = ((d1 q1 a), (d2 q2 a))

-- Machine that accepts the concatenation of the languages accepted by m1 and m2
catFSM :: (Eq a, Ord b) => FSM a -> FSM b -> FSM (a, [b])
catFSM (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
  correct q x = x ++ [s2 | elem q fs1]
  qs = qs1 >< (power qs2)
  s  = (s1, norm(correct s1 []))
  fs = [(q, x) | (q, x) <- qs, overlap x fs2]
  d (q, x) a = (q', norm(correct q' (d2' x a))) where
    q' = d1 q a
    d2' x2 a = [d2 q2 a | q2 <- x2]

-- Machine that accepts the Kleene star of the language accepted by m1
starFSM :: Ord a => FSM a -> FSM [a]
starFSM (qs1, s1, fs1, d1) = (qs, s, fs, d) where
  qs = power qs1
  s  = []
  fs = [[]] ++ [x | x <- qs, overlap x fs1]
  d xs a
    | xs == [] = norm [d1 s1 a]
    | otherwise = norm [d1 x a | x <- xs]


---------------- Bonus Features (for testing and experimenting) ------------

-- Unary set closure (where "set" = normalized list)
-- uclosure xs g == smallest set containing xs and closed under g
uclosure :: Ord a => [a] -> (a -> [a]) -> [a]
uclosure xs g = sort $ stable $ iterate close (xs, []) where
  stable ((fr,xs):rest) = if null fr then xs else stable rest
  close (fr, xs) = (fr', xs') where
    xs' = fr ++ xs
    fr' = norm $ filter (`notElem` xs') $ concatMap g fr

-- reachable m == the part of m that is reachable from the start state
reachable :: Ord a => FSM a -> FSM a
reachable m@(qs, s, fs, d) = (qs', s, fs', d) where
  qs' = uclosure [s] (\q -> map (d q) sigma)
  fs' = filter (`elem` fs) qs'

-- Change the states of an FSM from an equality type to Int 
-- and use an array lookup for the transition function
intify :: Eq a => FSM a -> FSM Int
intify (qs, s, fs, d) = ([0..n-1], s', fs', d') where
  n = length qs
  m = length sigma
  s'  = ind qs s
  fs' = map (ind qs) fs
  arr = listArray ((0,0), (n-1,m-1)) [ind qs (d q a) | q <- qs, a <- sigma]
  d' q a = arr ! (q, ind sigma a)
  ind (q':qs) q = if q == q' then 0 else 1 + ind qs q

reduce :: Ord a => FSM a -> FSM Int
reduce = intify . reachable

---- Regular expressions, along with input and output
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
    sp d Empty         = showString "@"
    sp d (Let c)       = showString [c]
    sp d (Union r1 r2) = showParen (d > 6) $  -- prec(Union) = 6
                         sp 6 r1 .
                         showString "+" .
                         sp 6 r2
    sp d (Cat r1 r2)   = showParen (d > 7) $  -- prec(Cat) = 7
                         sp 7 r1 .
                         sp 7 r2
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

-- Use constructions above to get reduced machine associated with regex
-- Warning: it can take a lot of time/memory to compute these for "big" regex's
-- We will see much better ways later in the course
re2fsm :: RegExp -> FSM Int
re2fsm Empty = emptyFSM
re2fsm (Let c) = letterFSM c
re2fsm (Union r1 r2) = reduce $ unionFSM (re2fsm r1) (re2fsm r2)
re2fsm (Cat r1 r2) = reduce $ catFSM (re2fsm r1) (re2fsm r2)
re2fsm (Star r1) = reduce $ starFSM (re2fsm r1)
