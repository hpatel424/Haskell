import Data.List
import Data.Array

-- Fixed alphabet, but everything below should work for any sigma!
sigma :: [Char]
sigma = "ab"

-- Finite state machines, now indexed by the type of their states
-- M = (states, start, finals, transitions)  
type FSM a = ([a], a, [a], a -> Char -> a)

-- Nondeterministic FSMs, indexed by their type of state
-- All states are normalized and the output of d is normalized
-- M = (states, starts, finals, transitions)  
type Trans a = a -> Char -> [a]
type NFSM a = ([a], [a], [a], Trans a)

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

-- delta* construction
star :: (a -> Char -> a) -> (a -> [Char] -> a)
star = foldl'

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

reduce :: Ord a => FSM a -> FSM Int
reduce = intify . reachable

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

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

-- Symmetric difference (xor)
symdiff :: (Eq a, Eq b) => FSM a -> FSM b -> FSM (a,b)
symdiff (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
  qs = qs1 >< qs2
  s  = (s1, s2)
  fs = [(q1, q2) | q1 <- qs1, q2 <- qs2, (elem q1 fs1) /= (elem q2 fs2)]
  d (q1, q2) a = ((d1 q1 a), (d2 q2 a))


-- Boolean binary operation on FSMs. Examples:
-- binopFSM (||) m1 m2 computes union machine
-- binopFSM (&&) m1 m2 computes intersection machine
-- binopFSM (/=) m1 m2 computes symmetric difference
type BoolOp = Bool -> Bool -> Bool

binopFSM :: (Eq a, Eq b) => BoolOp -> FSM a -> FSM b -> FSM (a,b)
binopFSM op (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
    qs = qs1 >< qs2
    s = (s1, s2)
    fs = [(q1, q2) | (q1, q2) <- qs, op (elem q1 fs1) (elem q2 fs2)]
    d (q1, q2) a = (d1 q1 a, d2 q2 a)

-- Reverse FSM to a NFSM. Output machine accepts reverse of language of input machine.
reverseFSM :: Eq a => FSM a -> NFSM a
reverseFSM (qs1, s1, fs1, d1) = (qs, s, fs, d) where
    qs = qs1
    s = fs1
    fs = [s1]
    d q a = [q' | q' <- qs, d1 q' a == q]

-- Reachable states of a NFSM (similar to reachable but on NFSMs)
nreachable :: Ord a => NFSM a -> [a]
nreachable m@(qs, s, fs, d) = qs' where
    qs' = uclosure s (\q -> concat (map (d q) sigma))


-- Minimize a FSM. Put all of the above together to compute the minimum machine for m
-- Minimize a FSM. Put all of the above together to compute the minimum machine for m
minimize :: Ord a => FSM a -> FSM a
minimize (qs, s, fs, d) = (qs', s', fs', d') where
   xor = (binopFSM (/=) (qs, s, fs, d) (qs, s, fs, d))
   rever = (reverseFSM xor)
   nreach = (nreachable rever)
   compl = [(q1, q2) | q1 <- qs, q2 <- qs, (notElem (q1, q2) nreach)]
   rep r = (minimum[q2| (q1, q2) <- compl, q1 == r])
   qs' = (norm [(rep q)| q <- qs])
   s' = (rep s)
   fs' = (intersect (norm [(rep q)| q <- qs]) fs)
   d' q a = (rep (d q a))


-- Test. For example, make sure that the minimal machine agrees with the original machine
-- on lots of input strings. Try for multiple machines.