
import Data.List (foldl')
import Data.Char (isUpper)

-- Tape head direction (left, right)
data Dir = L | R

-- Result of a single transition
data Res a = Halt | Accept | Step a Char Dir

-- Turing machine M = (Q, s, d). Q is states, s is start state, d is transition function
type TM a = ([a], a, a -> Char -> Res a)

{-
Here, the transition function takes a state q and read-head character a and returns either

1. Halt, if there is no transition from q on a
2. Accept, if the TM should accept the original input string
3. Step q' a' dir, if the TM should transition to state q' after overwriting the symbol a with a' and moving the read-head in direction dir.

Note that this formulation doesn't involve any final states, which are not needed since one of the possible transition results is Accept.
-}

--Part 1

accept :: TM a -> String -> Bool
accept (qs, q0, d) w = run w q0 w

run :: String -> a -> String -> Bool
run w1 q w2 =
  case  q (head w2) of
    Halt -> w1
    Accept -> w1
    Step q' c dir ->
      let w1' = if dir == L then tail w1 else w1 ++ [c]
          w2' = if dir == R then tail w2 else c:w2
      in run w1' q' (if null w2' then " " else w2')

-- Haskell function "accepting" L
abc :: String -> Bool
abc xs = let n = length $ takeWhile (=='a') xs
             block x = replicate n x
         in xs == concatMap block "abc"

--Part 2
compute :: TM a -> String -> String
compute (qs, q0, d) w = run "" q0 w

--reverse function
--compute rev w == reverse w
rev :: TM Int
rev = (qs, q0, d)
  where
    qs = [0..5]
    q0 = 0
    d 0 c = Step 1 c R
    d 1 c = Step 2 c R
    d 2 c = Step 3 c R
    d 3 c = Step 4 c R
    d 4 c = Step 5 c L
    d 5 c = Step 5 c L