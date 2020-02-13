{-# LANGUAGE MultiParamTypeClasses #-}

module Labels where

import Syntax
import qualified Embed

import Util
import FairStream

---------------------------------------

labelMap :: (l1 -> l2) -> GenStream s l1 -> GenStream s l2
labelMap f (Conj a l b) = Conj (labelMap f a) (f l) (labelMap f b)
labelMap f (Disj a b)   = Disj (labelMap f a) (labelMap f b)
labelMap f (Goal g s)   = Goal g s

instance (Labels l1 p1, Labels l2 p2) => Labels (l1, l2) (p1, p2) where
  new (p1, p2) s = (new p1 $ labelMap fst s, new p2 $ labelMap snd s)
  keep (p1, p2) (l1, l2) = (keep p1 l1, keep p2 l2)
  predicate (p1, p2) s (l1, l2) = predicate p1 (labelMap fst s) l1 && predicate p2 (labelMap snd s) l2
  update (p1, p2) s1 s2 (l1, l2) = (update p1 (labelMap fst s1) (labelMap fst s2) l1,
                                    update p2 (labelMap snd s1) (labelMap snd s2) l2)
  size (p1, p2) (l1, l2) = size p1 l1 + size p2 l2

---------------------------------------

instance Labels () () where
  new () _ = ()
  keep () () = ()
  predicate () _ () = True
  update () _ _ () = ()
  size _ _ = 0

---------------------------------------

instance Labels Int Int where
  new i _ = i
  keep _ n = n
  predicate _ _ n = n /= 0
  update _ _ _ n = n - 1
  size _ _ = 0

---------------------------------------

data Disj = D Int deriving Show

disjs (Conj _ (D d) _) = d
disjs (Disj a b)       = 1 + disjs a + disjs b
disjs _                = 0

instance Labels Disj Disj where
  new _ s = D $ disjs s
  keep _ _ = D 0
  predicate (D p) _ (D d) = d <= p
  update  _  _ s _ = D $ disjs s
  size _ _ = 0

---------------------------------------

data SignVars  = SV [(Int, Ts)] Int deriving Show
data SignVarsP = SVP [Int] Int

-- It desn't work
instance Labels SignVars SignVarsP where
  new (SVP l n) s = SV (map (\v -> (v, substInT a $ V v)) l) n where
    (_, a) = getLeftLeaf s
  keep _ l = l
  predicate _ s (SV v i) = i /= 0 || any (\(i, t) -> Embed.isStrictInst t $ substInT a $ V i) v where
    (_, a) = getLeftLeaf s
  update (SVP l n) _ s (SV _ 0) = SV (map (\v -> (v, substInT a $ V v)) l) n where
    (_, a) = getLeftLeaf s
  update _ _ _ (SV l n) = SV l (n-1)
  size _ (SV l _) = length l

---------------------------------------

newtype Streams = Streams [Stream Streams] deriving Show
type Comparator = Stream Streams -> Stream Streams -> Bool
data StreamsComparator = SC Comparator (Maybe Comparator)

sc1 :: Comparator -> StreamsComparator
sc1 = flip SC Nothing
sc2 :: Comparator -> Comparator -> StreamsComparator
sc2 f = SC f . Just

instance Labels Streams StreamsComparator where
  new _ _ = Streams []
  keep _ _ = Streams []
  predicate (SC f (Just g)) s (Streams ss@(x:y:_)) | g x y = not $ any (flip f s) ss
  predicate (SC _ (Just g)) s (Streams (x:_)) | g s x = True
  predicate (SC f _) s (Streams ss) = not $ any (flip f s) ss
  update _ s _ (Streams xs) = Streams $ s:xs
  size _ (Streams ss) = length ss