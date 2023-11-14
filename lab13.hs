import Data.List (foldl')
import Data.Char (isUpper)

-- CFG' G = (Start, Follows, Nullable)
type CFG' = (Char, Char -> Char -> [String], Char -> Bool)
                
accept' :: CFG' -> String -> Bool
accept' (s,d,e) = elem [] . foldl' (\xs c -> concatMap (lq c) xs) (close [s]) where
  close :: String -> [String]
  close [] = [""]
  close c@(x:xs) |(isUpper x) && (e x == True) = [c] ++ close xs
                 |otherwise = [c]
  lq a [] = []
  lq a (x:xs) | isUpper x = map (++ xs) $ concat $ map (close) (d x a)
              | otherwise = if a == x then [xs] else []

-- Balanced parentheses
-- Original:  S --> (S) | SS | ε
-- TNF + ε:   S --> (S) | (S)S  (S nullable)

bp' :: CFG'
bp' = ('S', d, e) where
  d 'S' '(' = ["S)", "S)S"]
  d 'S' ')' = []
  e 'S' = True

{-
ghci> accept' bp' "((((()))))"
True
ghci> accept' bp' ")()("      
False
ghci> accept' bp' "(()())()"
True
ghci> accept' bp' "(()())()("
False
ghci> accept' bp' ""         
True
-}

g1 :: CFG'
g1 = ('S', d, e) where
  d 'S' 'a' = ["Sb", "SbS"];
  d 'S' 'b' = ["Sa", "SaS"];  
  e 'S' = True;

{-
ghci> accept' g1 "aaaaaabbbbb"
False
ghci> accept' g1 "aaabbb"     
True
ghci>
ghci> accept' g1 "aa"    
False
ghci> accept' g1 ""  
True
ghci> accept' g1 "aabab"
False
ghci> accept' g1 "bababa"
True
ghci> accept' g1 "abab"  
True
-}

g5 :: CFG'
g5 = ('S', d, e) where
  d 'S' 'a' = ["S", "SbS"];
  d 'S' 'b' = [];  
  e 'S' = True;

{-
ghci> accept' g5 "abab"
False
ghci> accept' g5 "ababa"
True
ghci> accept' g5 "aaaaaa"
True
ghci> accept' g5 "bbbbb" 
False
ghci> accept' g5 ""     
True
ghci> accept' g5 "abbbbbba"
False
-}

-- CFG G = (Start, Follows)
type CFG = (Char, Char -> Char -> [String])

accept :: CFG -> [Char] -> Bool
accept (s,d) = elem [] . foldl' (\xs c -> concatMap (lq c) xs) [[s]] where
  lq a [] = []
  lq a (x:xs) | isUpper x = map (++ xs) $ d x a          -- nonterminal
              | otherwise = if a == x then [xs] else []  -- terminal


-- Example 1: Balanced parentheses (not including the empty string)
-- original: S --> () | (S) | SS
-- in TNF:   S --> () | ()S | (S) | (S)S
bp :: CFG
bp = ('S', d) where
  d 'S' '(' = [")", ")S", "S)", "S)S"]
  d 'S' ')' = []


--G6: S -> aBa | aSa 
-- B -> bB | b

g6 :: CFG
g6 = ('S', d) where
    d 'S' 'a' = ["Ba", "Sa"]
    d 'S' 'b' = []
    d 'B' 'a' = []
    d 'B' 'b' = ["B", ""]

{-
ghci> accept g6 ['a','a','a','b','a','b']
False
ghci> accept g6 ['a','a','a','b','b','b','a']
False
ghci> accept g6 ['a','a','a','b','b','a','a']
False
ghci> accept g6 ['a','a','a','b','a','a']    
False
ghci> accept g6 ['a','a','b','a','a']    
True
ghci> accept g6 ['a','a','b','b','b','b','b','a','a']
True
-}


{-
G2:
    R -> a+R | b+R | 0+R | 1+R | a*+R | b*+R | 0*+R | 1*+R | (R)*+R | (R)+R
    R -> aF+R | bF+R | 0F+R | 1F+R | a*F+R | b*F+R | 0*F+R | 1*F+R | (R)*F+R | (R)F+R
    R -> a | b | 0 | 1 | a* | b* | 0* | 1* | (R)* | (R)
    R -> aF | bF | 0F | 1F | a*F | b*F | 0*F | 1*F | (R)*F | (R)F
    F -> a | b | 0 | 1 | a* | b* | 0* | 1* | (R)* | (R)
    F -> aF | bF | 0F | 1F | a*F | b*F | 0*F | 1*F | (R)*F | (R)F
-}

g2 :: CFG
g2 = ('R', d) where
    d 'R' 'a' = ["+R", "*+R", "F+R", "*F+R", "", "*", "F", "*F"]
    d 'R' 'b' = ["+R", "*+R", "F+R", "*F+R", "", "*", "F", "*F"]
    d 'R' '0' = ["+R", "*+R", "F+R", "*F+R", "", "*", "F", "*F"]
    d 'R' '1' = ["+R", "*+R", "F+R", "*F+R", "", "*", "F", "*F"]
    d 'R' '(' = ["R)*+R", "R)+R", "R)*F+R", "R)F+R", "R)*", "R)", "R)*F", "R)F"]
    d 'R' _ = []
    d 'F' 'a' = ["", "*", "F", "*F"]
    d 'F' 'b' = ["", "*", "F", "*F"]
    d 'F' '0' = ["", "*", "F", "*F"]
    d 'F' '1' = ["", "*", "F", "*F"]
    d 'F' '(' = ["R)*", "R)", "R)*F", "R)F"]
    d 'F' _ = []

{-
ghci> accept g2 ['a','a','b','b','b','b','b','a','a']
True
ghci> accept g2 ['a','b']                            
True
ghci> accept g2 ['a','+','b']
True
ghci> accept g2 ['a','-','b']
False
ghci> accept g2 ['a','*','b']
True
ghci> accept g2 ['(','a','+','b',')','*','+','1']
True
ghci> accept g2 ['(','a','+','b',')','*','+']  
False
ghci> accept g2 ['*','a','b']
False
ghci> accept g2 ['+','a','b']
False
-}