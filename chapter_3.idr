module Main

import Data.Vect


-- add definitions   - type declaration -> Idris adds skeleton
-- case analysis     - skeleton         -> Idris adds pattern matching
-- expression search - hole             -> Idris adds an expression

-- or

-- type   - creating / inspecting a type
-- define - creating / breaking a definition into clauses
-- refine - improving by filling holes or making the type more precise

-- allLengths : List String -> List Nat

-- cursor over allLengths
-- ,-d - idris-add-clause:

-- allLengths : List String -> List Nat
-- allLengths xs = ?allLengths_rhs

-- ,-t - idris-type-at-point: "allLengths_rhs : List String -> List Nat"

-- cursor over xs
-- ,-c - dris-case-dwim:

-- allLengths : List String -> List Nat
-- allLengths [] = ?allLengths_rhs_1
-- allLengths (x :: xs) = ?allLengths_rhs_2

-- and now we can write the function

allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

-- :total allLengths checks if allLengths is total, which is a functions that is
-- guaranteed to produce a result with for any well-typed input, in finite time

-- so it has to have clauses that cover all possible well-typed inputs
-- all recursive calls converge on a base case

-- :doc or ,-h-d gives the doumentation of what's under the cursor

xor : Bool -> Bool -> Bool
-- ,d on xor followed by ,c on x
xor False y = False
xor True y = not y

-- we are saying with the types that both input and output will
-- have the same number of elements
-- and that elem could be ordered
total insert : Ord elem =>
      (x : elem) -> (xsSorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) = if x < y then x :: y :: xs
                              else y :: insert x xs

total inSort : Ord elem => Vect n elem -> Vect n elem
inSort [] = []
inSort (x :: xs) = let xsSorted = inSort xs in (insert x xsSorted)


-- exercises

total myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = 1 + myLength xs

total myReverse : List a -> List a
myReverse [] = []
myReverse (x :: xs) = (myReverse xs) ++ [x]

total myMap : (a -> b) -> List a -> List b
myMap f [] = []
myMap f (x :: xs) = f x :: myMap f xs

total myVectMap : (a -> b) -> Vect k a -> Vect k b
myVectMap f [] = []
myVectMap f (x :: xs) = f x :: myVectMap f xs


-- transpose converts rows to columns
-- given for ex
-- [
--   [1,2],
--   [3,4],
--   [5,6]
-- ]
-- gives back
-- [
--   [1,3,5],
--   [2,4,6]
-- ]
createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans

-- the first helper function is similar to the lib function zipWith
-- zipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
-- transposeHelper : (Vect n elem) -> (Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)


addMatrix : Num a => Vect n (Vect m a) ->
                     Vect n (Vect m a) ->
                     Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

