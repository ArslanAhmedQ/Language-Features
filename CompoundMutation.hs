{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module CompoundMutation (
    Mutable, get, set, def,
    Memory, Pointer(..), Value(..), Person(..), 
    StateOp(..), runOp, (>>>), (>~>), returnVal, 
    alloc, free,
    (@@), age, isStudent
    )
    where

import AList (AList, lookupA, insertA, updateA, containsA, removeA)

-- A type representing the possible values stored in memory.
-- data type Value to represent the different types of data that store and allow the user to change 
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values, an association list mapping numeric keys to Values 
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer | PersonP Integer Integer deriving Show  --one integer for age and other isStudent

-- A type representing a person with two attributes:
-- age and whether they are a student or not.
data Person = Person Integer Bool deriving Show

-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    --get :: Memory -> Pointer a -> a
    get :: Pointer a -> StateOp a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    --set :: Memory -> Pointer a -> a -> Memory
    set :: Pointer a -> a -> StateOp a

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    --def :: Memory -> Integer -> a -> (Pointer a, Memory)
    def :: Integer -> a -> StateOp (Pointer a)

instance Mutable Integer where
    {-|
    From before the changing to be in the terms of StateOp
    get mem (P pt) = if (containsA mem pt)
                     then case (lookupA mem pt) 
                          of IntVal val -> val
                     else error "Key not found"

    set mem (P pt) val = if (containsA mem pt)
                         then updateA mem(pt, IntVal val)
                         else error "Key not found"

    def mem key val = if (containsA mem key)
                      then error "Key already defined"
                      else (P key, insertA mem (key, IntVal val))
    -}
    
    -- |Returns the integer that is contained at the address pointed to in a given memory by a given pointer
    -- Error is thrown if key is not found
    get (P pt) = StateOp (\mem -> ((if (containsA mem pt)
                                  then case (lookupA mem pt) 
                                       of IntVal val -> val
                                  else error "Key not found"), mem))
    -- |Returns the new integer value at the pointer location and the new memory that that was changed. 
    -- Error is thrown if key is not found
    set (P pt) val = StateOp (\mem -> (val, (if (containsA mem pt)
                                           then updateA mem (pt, IntVal val)
                                           else error "Key not found")))
    -- |Returns a pointer to where a new integer was inserted in memory. Also return the new modified memory.  
    -- Error is thrown if memory key is already used in memory
    def key val = StateOp (\mem -> (if (containsA mem key)
                                   then error "Key already defined"
                                   else (P key, insertA mem (key, IntVal val))))

instance Mutable Bool where
    {-|
    From before the changing to be in the terms of StateOp
    get mem (P pt) = if (containsA mem pt)
                     then case (lookupA mem pt) 
                          of BoolVal val -> val
                     else error "Key not found"

    set mem (P pt) val = if (containsA mem pt)
                         then updateA mem(pt, BoolVal val)
                         else error "Key not found"

    def mem key val = if (containsA mem key)
                      then error "Key already defined"
                      else (P key, insertA mem (key, BoolVal val))
    -}
    
    -- |Returns the boolean that is contained at the address pointed to in a given memory by a given pointer
    -- Error is thrown if key is not found
    get (P pt) = StateOp (\mem -> ((if (containsA mem pt)
                                  then case (lookupA mem pt) 
                                       of BoolVal val -> val
                                  else error "Key not found"), mem))
    -- |Returns the new boolean set at the pointer location and the new memory that that was changed.   
    -- Error is thrown if key is not found
    set (P pt) val = StateOp (\mem -> (val, (if (containsA mem pt)
                                           then updateA mem (pt, BoolVal val)
                                           else error "Key not found")))
    -- |Returns a pointer to where a new boolean was inserted in memory. Also return the new modified memory.  
    -- Error is thrown if memory key is already used in memory
    def key val = StateOp (\mem -> (if (containsA mem key)
                                   then error "Key already defined"
                                   else (P key, insertA mem (key, BoolVal val))))
    
-- Part2: Chaining    -- implement get, set and def in terms of this 
data StateOp a = StateOp (Memory -> (a, Memory))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

-- |then 
--  Combines two stateOp operations into one
(>>>) :: StateOp a -> StateOp b -> StateOp b  
op1 >>> op2 = StateOp (\mem -> let (_, mem1) = runOp op1 mem
                               in runOp op2 mem1)

-- |bind
-- Binds two operations together.
(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b 
f >~> g = StateOp (\mem -> let (x, mem1) = runOp f mem
                               newStackOp = g x
                           in runOp newStackOp mem1)
                           
-- |return
--  returns the value as the first element in the tuple
returnVal :: a -> StateOp a
returnVal a = StateOp (\mem -> (a, mem))





--- part 4 Safety improvements 

-- This helper function generate a key which does not exist in recent memory 
newKey memory key = if (containsA memory key)
                    then (newKey memory (key + 1))
                    else key 
             
-- this function automatically generates a new key to bind in the value in memory 
-- Uses a helper function which checks a key exists in memory or not
-- return a different key if exist otherwise the same key 
alloc :: Mutable a => a -> StateOp (Pointer a)
alloc a = StateOp(\mem -> runOp (def (newKey mem 0) a ) mem )  -- it could also start with 1, key is unique 



-- |Frees an address in memory
-- Returns the new memeory which is similar to the old one but has the key and its value removed
free :: Mutable a => Pointer a -> StateOp ()
free (P a) = StateOp (\mem -> 
  if containsA mem a
  then ((), removeA mem a)
  else error "No key")

--free (P loc)= StateOp(\mem -> ((), filter((/=loc).fst) mem ))



--Part 05 Implementing functions 

-- Extracts the required attribute from the Person Pointer
(@@) :: Pointer a -> (Pointer a -> Pointer b) -> Pointer b
person_pointer @@ attribute = attribute person_pointer

-- This function pulls out age from person pointer
age :: Pointer Person -> Pointer Integer
age (PersonP x y) = P x
----isStudent a = let personP x _ = a
            --in P x 

-- This function pulls out isStudent from person pointer
isStudent :: Pointer Person -> Pointer Bool
isStudent (PersonP x y) = P y
--isStudent s = let personP _ x = s
            -- in P x  

instance Mutable Person where
    -- | gets the age of person and whether they are a student.
    -- An error is thrown if either keys of the person's attributes are not found
    get (PersonP pt1 pt2) = StateOp (\mem -> ((if (containsA mem pt1) && (containsA mem pt2) 
                                             then (Person (case (lookupA mem pt1) 
                                                            of IntVal val -> val) 
                                                          (case (lookupA mem pt2) 
                                                            of BoolVal val -> val))
                                             else error "Key not found"), mem))
    
    -- |Updates a person's age and/or student status in memory.
    -- An error is thrown if either keys of the person's attributes are not found
    set (PersonP pt1 pt2) (Person val1 val2) = StateOp (\mem -> ((Person val1 val2), (if (containsA mem pt1) && (containsA mem pt2)
                                                                                      then let newMem1 = updateA mem (pt1, IntVal val1)
                                                                                           in updateA newMem1 (pt2, BoolVal val2)
                                                                                      else error "Key not found")))
    -- | Creates a new entry in memory for a person with an age and whether he/she is a student.
    -- An error is thrown if the person's age key already exists in memory
    def key (Person val1 val2) = StateOp (\mem -> (if (containsA mem key)
                                                   then error "Key already defined"
                                                   else let ((P key2), newMem) = runOp (def key val1 >>> alloc val2) mem
                                                        in (PersonP key key2, newMem)))
-- for testing
personTest :: Person -> Integer -> StateOp (Integer, Bool, Person)
personTest person x =
    -- not using alloc, but we could
    def 1 person >~> \personPointer ->
    get (personPointer @@ age) >~> \oldAge ->
    set (personPointer @@ age) x >>>
    get (personPointer @@ isStudent) >~> \stu ->
    get (personPointer @@ age) >~> \newAge ->
    set personPointer (Person (2 * newAge) (not stu)) >>>
    get personPointer >~> \newPerson ->
    get (personPointer @@ isStudent) >~> \newStu ->
    returnVal (oldAge, newStu, newPerson)
  


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
