{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer(..), Value(..), 
    StateOp(..), runOp, (>>>), (>~>), returnVal,
    alloc, free --part4 
    )
    where

import AList (AList, lookupA, insertA, updateA)

-- A type representing the possible values stored in memory.
-- data type Value to represent the different types of data that store and allow the user to change 
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values, an association list mapping numeric keys to Values 
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer deriving Show 



-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    get :: Memory -> Pointer a -> a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Memory -> Pointer a -> a -> Memory

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Memory -> Integer -> a -> (Pointer a, Memory)

instance Mutable Integer where

    get mem (P pt) = (lookupA mem pt) of IntVal val -> val

    set mem (P pt) val = updateA mem(pt, IntVal val)

    def mem key val = (P key, insertA mem (key, IntVal val))

instance Mutable Bool where

    get mem (P pt) = (lookupA mem pt) of Boolval val -> val

    set mem (P pt) val = updateA mem(pt, BoolVal val)

    def mem key val = (P key, insertA mem (key, BoolVal val))
-- Part2: Chaining    -- implement get, set and def in terms of this 
data StateOp a = StateOp (Memory -> (a, Memory))

runOp :: StateOp a -> StateOp b -> StateOp b
runOp (StateOp op) mem = op mem

-- documents
(>>>) :: StateOp a -> StateOp b -> StateOp b
op1 >>> op2 = StateOp (\mem -> let (_, mem1) = runOp op1 mem
                                in runOp op2 mem1)


-- documents 
(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
arg1 >~> arg2 = StateOp (\mem -> let (x, mem1) = runOp f mem
                                    newStackOp = arg2 x
                                 in runOp newStackOp mem1)


returnVal :: a -> StateOp a
returnVal a = StateOp (\mem -> (a,mem))

-- part 4 Safety improvements 

-- This helper function generate a key which does not exist in recent memory 
newKey memory key = if (containsA memory key)
                    then (newKey memory (key + 1))
                    else key 
             
-- this function automatically generates a new key to bind in the value in memory 
-- Uses a helper function which checks a key exists in memory or not
-- return a diiferent key if exist otherwise the same key 
alloc :: Mutable a => a -> StateOp (Pointer a)
alloc a = StateOp(\mem -> runOp (def (newKey mem 0) a ) mem )  -- it could also start with 1, key is unique 






free :: Mutable a => Pointer a -> StateOp ()
free a = StateOp(\mem -> ((), let P pointer = p 
                                      in filter((/=pointer) . fst) mem )



f :: Integer -> StateOp Bool
f x =
   def 1 4 >~> \p1 ->
   def 2 True >~> \p2 ->
   set p1 (x + 5) >>>
   get p1 >~> \y ->
   set p2 (y > 3) >>>
   get p2

g :: Integer -> StateOp Integer
g x =
  def 1 (x + 4) >~> \p ->
  get p >~> \y ->
  returnVal (x * y)




