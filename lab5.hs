-- CSci 119, Lab 5
-- Reference: Lecture notes, Sections 4.1, 4.2

import Data.List (isInfixOf, isSuffixOf)  -- useful for testing in Part 2

-- Again, for this (and most) labs, the alphabet will be {a,b} to keep testing
-- easier, but your code should work just as well for any finite list.
sigma = ['a', 'b']

-- Finite State Machine M = (Q, s, F, d) with integer states
type FSM = ([Int], Int, [Int], Int -> Char -> Int)


---------------- Part 1: Representing FSMs

-- Check whether a finite state machine (qs, s, fs, d) is correct/complete:
-- (1) States qs are unique (no duplicates)
-- (2) Start state is a state (s is in qs)
-- (3) Final states are states (fs is a subset of qs)
-- (4) Transition function d gives a state in qs for every state in qs and
--     letter from sigma

qsUnique :: [Int] -> Bool
qsUnique [] = True
qsUnique (x:xs) = not (elem x xs) && qsUnique xs

isSubset :: [Int] -> [Int] -> Bool
isSubset [] _ = True
isSubset _ [] = False
isSubset (f:fs) xs = (elem f xs) && (isSubset fs xs) 

transition :: [Int] -> (Int -> Char -> Int) -> Bool
transition [] _ = True
transition qs d = and [elem (d q a) qs | q <- qs, a <- sigma]

-- Check for finite state machine
checkFSM :: FSM -> Bool
checkFSM (qs, s, fs, d) = (qsUnique qs) && (elem s qs) && (isSubset fs qs) && (transition qs d)

-- Gives the delta* function (recursive in w)
dstar :: FSM -> Int -> [Char] -> Int
dstar m q [] = q 
dstar (m@(_,_,_, d)) q (w:ws) = dstar m (d q w) ws

{-
ghci> dstar oddbs 3 "abb"
3
ghci> dstar oddbs 2 "ab"
-1
-}


-- Machine acceptance, Definition 1 (via delta*)
accept1 :: FSM -> [Char] -> Bool
accept1 (m@(_, s, fs, _)) w = elem (dstar m s w) fs

{-
ghci> accept1 oddbs "aba"
True
ghci> accept1 oddbs "baaa"
True
ghci> accept1 oddbs "bbabb"
False
ghci> accept1 oddbs "aaaabbbb"
False
-}

-- Machine acceptance, Definition 2 (via L_q(M))
accept2 :: FSM -> [Char] -> Bool
accept2 (qs, s, fs, d) w = aux s w where
  -- aux q w = whether the machine, starting in q, accepts w (recursive in w)
  aux :: Int -> [Char] -> Bool
  aux q [] = elem q fs
  aux q (w:ws) = aux (d q w) ws

{-
ghci> accept2 oddbs "ababab"
True
ghci> accept2 oddbs "aba"
True
ghci> accept2 oddbs "bbbb"
False
ghci> accept2 oddbs "abab"
False
-}


---------------- Part 2: FSM construction

-- Define a machine that accepts exactly the strings with an odd number of b's
-- (Solution given for illustration)
oddbs :: FSM
oddbs = ([0,1], 0, [1], d) where
  d q 'a' = q        -- stay in the same state
  d q 'b' = 1 - q    -- switch states

-- Define a machine that accepts exactly the strings that do not contain "aab"
-- as a substring and test it adequately
avoid_aab :: FSM
avoid_aab = ([0..3], 0, [0..2], d) where
  d 0 'a' = 1
  d 1 'a' = 2
  d 2 'a' = 2
  d 2 'b' = 3
  d 3 _ = 3
  d _ _ = 0

-- Define a machine that accepts all strings that end in "ab" and test
end_ab :: FSM
end_ab = ([0..2], 0, [2], d) where
  d 0 'b' = 0
  d 0 'a' = 1
  d 1 'a' = 1
  d 1 'b' = 2
  d 2 'a' = 1
  d 2 'b' = 0
  d q _ = 0   --Takes care of other letters

-- Define a function that takes a string and returns a machine that accepts
-- exactly that string and nothing else (compare to 'onestr' from Lab 3)
-- Hint: the expression w !! i gives the i-th character of the string w
exactly :: String -> FSM
exactly w = ([0..(length w + 1)], 0, [length w], d) where
  d q a
    | (q < length w) && (a == (w !! q)) = q + 1
    | (q == (length w + 1)) = q
    | otherwise = (length w + 1)


-- Test the machines above. Also, try out the exerices at the end of Section 3.2
-- in my notes as FSMs. Do they produce the same results as your RegExp's did?

-- Recursive reimplementation of strings function from Lab 4
strings :: Int -> [String]
strings n = concat [strs i | i <- [0..n]] where
  strs 0 = [""]
  strs n = [a:xs | a <- sigma, xs <- strs (n-1)]

s10 = strings 10  -- or larger, if you like

-- Example test for the oddbs machine above
oddbs_test = and [accept1 oddbs w == odd (num_bs w) | w <- s10] where
  num_bs w = sum (map (\x -> if x == 'b' then 1 else 0) w)
  
