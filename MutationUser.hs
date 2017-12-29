{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}


module MutationUser (
    pointerTest, 
    swap, 
    swapCycle
    )
    where 

import Mutation (
    Mutable, get, set, def,
    Memory, Pointer(..), Value(..), 
    StateOp(..), runOp, (>>>), (>~>), returnVal,
    alloc, free --part4 
    )

import AList (AList, lookupA, insertA, updateA, containsA, removeA)



-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the StateOp of the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
pointerTest :: Integer -> StateOp (Pointer Integer, Pointer Bool)
pointerTest n = (def 100 (n + 3)) >~> \x ->
                (def 500 (n > 0)) >~> \y ->
                returnVal (x, y)



--Part 3 CALLING WITH REFERENCES 

-- given two pointers, swap the values of each other and return the respected one 
swap:: Mutable a => Pointer a -> Pointer a -> StateOp ()
swap pointer1 pointer2 =
    --let stackOp = get pointer2 in
    get pointer1 >~> \x -> 
    get pointer2 >~> \y -> 
    set pointer2 x >>>
    set pointer1 y >>>
    returnVal ()

  

--  the function takes a list of pointers p1, ..., pn, with corresponding values v1, ..., vn,
--  and modify p1’s value to v2, and so on. basically changing every pointer's value. 
-- if length less than 2, no change occured 
swapCycle :: Mutable a => [Pointer a] -> StateOp ()

swapCycle [] = returnVal ()
swapCycle (pt1:[]) = returnVal()
swapCycle (pt1:pt2:rest) = swap pt1 pt2 >>> swapCycle (pt2:rest)
