-- CSci 119, Lab 4

import Data.List (sort, stripPrefix) -- for your solution to Lab 3


---------------- Code provided to you in Lab 3 ----------------

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Length-Ordered Lists over "character type" a (aka "strings")
-- Invariant: In LOL n xs, n == length xs
data LOL a = LOL Int [a] deriving (Eq,Ord)

instance Show a => Show (LOL a) where
  show (LOL n xs) = show xs

-- Empty list (epsilon)
eps :: LOL a
eps = LOL 0 []

-- Smart constructor for LOL a, establishes invariant
lol :: [a] -> LOL a
lol xs = LOL (length xs) xs

-- Normalized lists of LOLs (aka "languages")
-- Invariant: xs :: Lang a implies xs is sorted with no duplicates
type Lang a = [LOL a]

-- Smart constructor for (finite) languages
lang :: Ord a => [[a]] -> Lang a
lang xs = norm $ map lol xs

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


---------------- Your solution to Lab 3 ----------------

-- Include here any code you need from your solution to Lab 3 for testing
-- purposes. After a few days, I will release a solution for you to use.
-- Don't duplicate the code just given above.



---------------- Part 1 ----------------

-- Implement the seven RECURSIVE predicates/operations on RegExp given in
-- Section 3.3 of the notes; call them empty, uni, byp, inf, revrs, lq,
-- and nep. Each should begin with a type declaration. Include several tests
-- for each function. Then implement one more not given there: purity.

--empty
empty :: RegExp -> Bool
empty (Empty) = True
empty (Let a) = False
empty (Union r1 r2) = (empty r1) && (empty r2)
empty (Cat r1 r2) = (empty r1) || (empty r2)
empty (Star r1) = False

{-
ghci> empty (Let 'a')
False
ghci> empty Empty
True
ghci> empty (Union(Let 'a') (Let 'a'))
False
ghci> empty (Union(Let 'a') Empty)    
False
ghci> empty (Cat (Let 'a') Empty)
True
ghci> empty (Cat (Let 'a') (Let 'a'))
False
ghci> empty (Star (Let '*'))
False
ghci> empty (Star Empty)
False
-}

--uni
uni :: RegExp -> Bool
uni (Empty) = False
uni (Let a) = False
uni (Union r1 r2) = (uni r1 && empty r2) || (empty r1 && uni r2) || (uni r1 && uni r2)
uni (Cat r1 r2) = (uni r1) && (uni r2)
uni (Star r1) = (empty r1) || (uni r1)

{-
ghci> uni Empty
False
ghci> uni  (Let 'a')
False
ghci> uni (Union (Let 'a') (Let 'b'))
False
ghci> uni (Cat (Let 'a') (Let 'b'))  
False
ghci> uni (Star (Let 'a'))         
False
-}

--byp
byp :: RegExp -> Bool
byp (Empty) = False
byp (Let a) = False
byp (Union r1 r2) = (byp r1) || (byp r2)
byp (Cat r1 r2) = (byp r1) && (byp r2)
byp (Star r1) = True

{-
ghci> byp Empty
False
ghci> byp (Let 'a')
False
ghci> byp (Union (Let 'a') (Let 'b'))
False
ghci> byp (Cat (Let 'a') (Let 'b'))
False
ghci> byp (Star (Let 'a'))
True
-}

--inf 
inf :: RegExp -> Bool
inf (Empty) = False
inf (Let a) = False
inf (Union r1 r2) = (inf r1) || (inf r2)
inf (Cat r1 r2) = (inf r1 && not (empty r2)) || (inf r2 && not (empty r1))
inf (Star r1) = (not (empty r1)) && not (uni r1)

{-
ghci> inf Empty
False
ghci> inf (Let 'a')
False
ghci> inf (Cat (Let 'a') (Let 'b'))
False
ghci> inf (Cat Empty (Let 'a'))
False
ghci> inf (Star (Let 'a'))
True
-}

--revrs
revrs :: RegExp -> RegExp
revrs (Empty) = Empty
revrs (Let a) = Let a
revrs (Union r1 r2) = Union (revrs r1) (revrs r2)
revrs (Cat r1 r2) = Cat (revrs r2) (revrs r1)
revrs (Star r1) = Star (revrs r1)

{-
ghci> revrs Empty
Empty
ghci> revrs (Let 'a')
Let 'a'
ghci> revrs (Union (Let 'a') (Let 'b'))          
Union (Let 'a') (Let 'b')
ghci> revrs (Cat (Let 'a') (Let 'b'))  
Cat (Let 'b') (Let 'a')
ghci> revrs (Star (Let 'a'))         
Star (Let 'a')
-}

--lq
lq :: Char -> RegExp -> RegExp
lq s Empty = Empty
lq s (Let a) = if s == a then Star Empty else Empty
lq s (Union r1 r2) = Union (lq s r1) (lq s r2)
lq s (Cat r1 r2) = if byp r1 then Union (Cat (lq s r1) r2) (lq s r2) else Cat (lq s r1) r2
lq s (Star r1) = Cat (lq s r1) (Star r1)

{-
ghci> lq 'a' Empty
Empty
ghci> lq 'a' (Let 'b')
Empty
ghci> lq 'a' (Let 'a')
Star Empty
ghci> lq 'a' (Union (Let 'b') (Let 'c'))
Union Empty Empty
ghci> lq 'a' (Cat (Let 'b') (Let 'c'))  
Cat Empty (Let 'c')
ghci> lq 'a' (Star (Let 'b'))
Cat Empty (Star (Let 'b'))
-}

--nep
nep :: RegExp -> RegExp
nep (Empty) = Empty
nep (Let a) = Let a
nep (Union r1 r2) = Union (nep r1) (nep r2)
nep (Cat r1 r2) = Union (Cat (nep r1) r2) (Cat r1 (nep r2))
nep (Star r1) = Cat (nep r1) (Star r1)

{-
ghci> nep Empty
Empty
ghci> nep (Let 'a')
Let 'a'
ghci> nep (Union (Let 'a') (Let 'b'))
Union (Let 'a') (Let 'b')
ghci> nep (Cat (Let 'a') (Let 'b'))  
Union (Cat (Let 'a') (Let 'b')) (Cat (Let 'a') (Let 'b'))
ghci> nep (Star (Let 'a'))
Cat (Let 'a') (Star (Let 'a'))
-}

-- Purity. A regular expression is *pure* if every string matching r
-- has at least one character, or, equivalently, if ε is not in [[r]].
-- (Note that r is pure exactly when r is not bypassable, but you are
-- to give a recursive definition here, not define it in terms of byp.)

purity :: RegExp -> Bool
purity (Empty) = False
purity (Let a) = True
purity (Union r1 r2) = (purity r1) && (purity r2)
purity (Cat r1 r2) = (purity r1) || (purity r2)
purity (Star r1) = False

{-
ghci> purity Empty
False
ghci> purity (Let 'a')
True
ghci> purity (Union (Let 'a') (Let 'b'))
True
ghci> purity (Cat (Let 'a') (Let 'b'))  
True
ghci> purity (Star (Let 'a'))
False
-}

---------------- Part 2 ----------------

-- Implement the two matching algorithms given in Section 3.4 of the notes,
-- where the second algorithm is the modified one I posted on Piazza (@96).
-- Call them match1 and match2. Start by implementing splits:

-- splits xs = list of all possible splits of xs, in order. For example,
-- splits "bc" =  [("","bc"), ("b","c"), ("bc","")]
-- splits "abc" = [("","abc"), ("a","bc"), ("ab","c"), ("abc","")]
splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits [x] = [([], [x]), ([x], [])]
splits (xs) = [(take n xs, drop n xs) | n <- [0..length xs]]

{-
ghci> splits "ab" 
[("","ab"),("a","b"),("ab","")]
ghci> splits "car"
[("","car"),("c","ar"),("ca","r"),("car","")]
-}


match1 :: RegExp -> String -> Bool
match1 Empty w = False
match1 (Let a) "" = False
match1 (Let a) (x:w) = a == x
match1 (Union r1 r2) w = match1 r1 w || match1 r2 w
match1 (Cat r1 r2) w = or [match1 r1 w1 && match1 r2 w2 | (w1,w2) <- splits w]
match1 (Star r) w = (w == "") || or [(w1 /= "") && match1 r w1 && match1 (Star r) w2 | (w1, w2) <- splits w]

{-
ghci> match1 Empty "a"    
False
ghci> match1 (Let 'a') ""
False
ghci> match1 (Let 'a') "a"
True
ghci> match1 (ab) "aa"
True
ghci> match1 (Let 'a') "aa"
True
ghci> match1 (Union ttla ab) "aa"
True
ghci> match1 (Union ttla ab) "abab"
True
ghci> match1 (Cat ttla ab) "a"     
False
ghci> match1 (Cat ttla ab) "abab"
True
ghci> match1 (Cat ttla ab) "bab" 
False
ghci> match1 (Star Empty) "a"   
False
ghci> match1 (Star (ab)) "aa"
True
-}


match2 :: RegExp -> String -> Bool
match2 rs w = match2helper [rs] w where
   match2helper :: [RegExp] -> String -> Bool
   match2helper [] w = w == ""
   match2helper (Empty:rs) w = False
   match2helper ((Let a):rs) "" = False
   match2helper ((Let a):rs) (x:w) = ((a:w) == (x:w)) && (match2helper rs w)
   match2helper ((Union r1 r2):rs) w = (match2helper (r1:rs) w) || (match2helper (r2:rs) w)
   match2helper ((Cat r1 r2):rs) w = (match2helper (r1:r2:rs) w) || ((byp r1) &&(match2helper (r2:rs) w))
   match2helper ((Star r1):rs) w =  ((match2helper rs w )) ||  (match2helper ((nep r1):(Star r1):rs) w)

{-
ghci> match2 Empty "abcd"
False
ghci> match2 Empty ""
False
ghci> match2 Empty "aabb"
False
ghci> match2 (Let 'a') ""
False
ghci> match2 (Union ab ttla) "bbab"
False
ghci> match2 (Cat ttla ab) "aaaab"   
True
ghci> match2 (Cat ttla ab) "babba"
False
ghci> match2 (Star (Empty)) "a"   
False
ghci> match2 (Star (ab)) "bb"  
True
-}



-- Some regular expressions for testing. Also, test them on other solutions
-- to the exercises of Section 3.2 (as many as you can get). 

sigma = ['a', 'b']                -- Alphabet used in all examples below

ab   = toRE "aa.bb.+*"            -- every letter is duplicated
ttla = toRE "ab+*a.ab+.ab+."      -- third to last letter is a
ena  = toRE "b*a.b*.a.*b*."       -- even number of a's
bb1  = toRE "aba.+*b.b.aab.+*."   -- contains bb exactly once


-- For your tests, you may also find the following helpful. For example,
-- you can generate all strings of length 10 (or 20) or less and then test
-- to see whether match1 r w == memb (lol w) (lang_of r) for each such w.

-- All strings on sigma of length <= n (useful for testing)
strings :: Int -> [String]
strings n = concatMap str [0..n] where
  str 0 = [""]
  str n = [a:w | a <- sigma, w <- str (n-1)]
