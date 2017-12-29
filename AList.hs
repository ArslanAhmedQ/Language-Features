{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}
module AList (
    AList,
    lookupA,
    insertA,
    updateA,  --might need a key exist here
    containsA,
    removeA
    )
    where


type AList a b = [(a, b)]

-- | Returns the value in the association list corresponding to the given key.
--   Assumes that the key is in the association list.
lookupA :: Eq a => AList a b -> a -> b
lookupA alist key = if (fst(head alist) == key)
                    then (snd(head alist)) 
                    else lookupA(tail alist) key

-- | Returns a new association list which is the old one, except with 
--   the new key-value pair inserted. However, it returns the *same* list
--   if the key already exists in the list.
insertA :: Eq a => AList a b -> (a, b) -> AList a b
insertA [] (key, val) = [(key, val)]
insertA alist (key, val) = if (fst(head alist) == key)
                           then alist
                           else (fst(head alist), snd(head alist)): (insertA (tail alist) (key, val))

-- | Returns a new association list which is the old one, except with 
--   the value corresponding to the given key changed to the given new value.
--   However, it returns the *same* list if the key doesn't appear in the list.
updateA :: Eq a => AList a b -> (a, b) -> AList a b
updateA  [] (key, val)= []
updateA [(x,y)] (key, val) = if (x == key) 
                             then [(x, val)] 
                             else [(x,y)]

updateA alist (key, val) = if (fst(head alist)  == key) 
                           then (fst(head alist), val):(tail alist)
                           else (fst(head alist), snd(head alist)): (updateA (tail alist) (key, val))
                           

-- Returns whether a specified key exists in a given association list
containsA :: Eq a => AList a b -> a -> Bool
containsA [] _ = False
containsA alist key = if (fst(head alist) == key)
                      then True
                      else containsA(tail alist) key

-- | Returns a new association list which is the old one, except with 
--   the key/value pair corresponding to the given key is removed from the association list.
--   However, it returns the *same* list if the key doesn't appear in the list.
removeA :: Eq a => AList a b -> a -> AList a b
removeA alist key =
  filter ((/= key).fst) alist  --filter(pred, list)

----free (P loc)= StateOp(\mem -> ((), filter((/=loc).fst) mem ))