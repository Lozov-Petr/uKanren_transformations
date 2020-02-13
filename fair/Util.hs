module Util where

import Syntax
import FairStream

---------------------------------------

getLeftLeaf :: GenStream s l -> (Goal, s)
getLeftLeaf (Disj a _  ) = getLeftLeaf a
getLeftLeaf (Conj a _ _) = getLeftLeaf a
getLeftLeaf (Goal g s) = (g, s)

height :: GenStream a b -> Int
height (Conj a _ b) = 1 + max (height a) (height b)
height (Disj a b  ) = 1 + max (height a) (height b)
height _            = 0

path :: GenStream a b -> Int
path (Conj a _ _) = 1 + path a
path (Disj a _  ) = 1 + path a
path _            = 0

sizeStream :: GenStream a b -> Int
sizeStream (Conj a _ b) = 1 + sizeStream a + sizeStream b
sizeStream (Disj a b)   = 1 + sizeStream a + sizeStream b
sizeStream _            = 1

disjCount :: GenStream a b -> Int
disjCount (Conj a _ _) = disjCount a
disjCount (Disj a b)   = 1 + disjCount a + disjCount b
disjCount _            = 0

conjCount :: GenStream a b -> Int
conjCount (Conj a _ b) = 1 + conjCount a + conjCount b
conjCount (Disj a b)   = conjCount a + conjCount b
conjCount _            = 0

disjsInConjs :: GenStream a b -> [Int]
disjsInConjs (Conj a _ _) = [disjCount a]
disjsInConjs (Disj a b)   = disjsInConjs a ++ disjsInConjs b
disjsInConjs _            = []

substInT :: Subst -> Ts -> Ts
substInT s (V x) =
  case lookup x $ snd s of
    Nothing -> V x
    Just t  -> substInT s t
substInT s (C n a) = C n $ map (substInT s) a

substInG :: Subst -> Goal -> Goal
substInG s (t1 :=: t2)  = substInT s t1 :=: substInT s t2
substInG s (Invoke n a) = Invoke n $ map (substInT s) a
substInG s (a :\/: b)   = substInG s a :\/: substInG s b
substInG s (a :/\: b)   = substInG s a :/\: substInG s b

substInS :: Stream l -> HoleStream l
substInS (Goal g s)   = Goal (substInG s g) ()
substInS (Disj a b)   = Disj (substInS a) $ substInS b
substInS (Conj a l b) = Conj (substInS a) l b

maxSizeLabels :: Labels l p => p -> GenStream a l -> Int
maxSizeLabels p (Conj a l b) = maximum [size p l, maxSizeLabels p a, maxSizeLabels p b]
maxSizeLabels p (Disj a b)   = max (maxSizeLabels p a) $ maxSizeLabels p b
maxSizeLabels _ _            = 0