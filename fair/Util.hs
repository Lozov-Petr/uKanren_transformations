module Util where

import Syntax
import FairStream

getLeftLeaf :: GenStream s l -> (Goal, s)
getLeftLeaf (Disj a _  ) = getLeftLeaf a
getLeftLeaf (Conj a _ _) = getLeftLeaf a
getLeftLeaf (Goal g s) = (g, s)

high :: GenStream a b -> Int
high (Conj a _ _) = 1 + high a
high (Disj a _  ) = 1 + high a
high _            = 0

size :: GenStream a b -> Int
size (Conj a _ b) = 1 + size a + size b
size (Disj a b)   = 1 + size a + size b
size _            = 1

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

getTerm :: Subst -> Ts -> Ts
getTerm s (V x) =
  case lookup x $ snd s of
    Nothing -> V x
    Just t  -> getTerm s t
getTerm s (C n a) = C n $ map (getTerm s) a