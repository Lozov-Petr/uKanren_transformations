{-# LANGUAGE MultiParamTypeClasses #-}

module Labels where

import Syntax
import qualified Embed

import Util
import FairStream
import FairEval

---------------------------------------

instance Labels () () where
  new       () _    = ()
  keep      ()   () = ()
  predicate () _ () = True
  update    () _ () = ()

---------------------------------------

instance Labels Int Int where
  new       i _   = i
  keep      _   n = n
  predicate _ _ n = n /= 0
  update    _ _ n = n - 1

---------------------------------------

data Disj = D Int Int deriving Show

disjs (Conj _ (D d _) _) = d
disjs (Disj a b)         = 1 + disjs a + disjs b
disjs _                  = 0

instance Labels Disj Disj where
  new       (D _ i) s         = D (disjs s) i
  keep      _         (D _ n) = D 0 n
  predicate (D p _) _ (D d n) = n /= 0 && d <= p
  update    _       s (D _ n) = D (disjs s) (n - 1)

---------------------------------------

data SignVars  = SV [(Int, Ts)] Int Int deriving Show
data SignVarsP = SVP [Int] Int Int

-- It desn't work
instance Labels SignVars SignVarsP where
  new (SVP l n m) s = SV (map (\v -> (v, getTerm a $ V v)) l) n m where
    (_, a) = getLeftLeaf s
  keep _ l = l
  predicate _ s (SV v i j) = j /= 0 && (i /= 0 || any (\(i, t) -> Embed.isStrictInst t $ getTerm a $ V i) v) where
    (_, a) = getLeftLeaf s
  update (SVP l n _) s (SV _ 0 m) = SV (map (\v -> (v, getTerm a $ V v)) l) n m where
    (_, a) = getLeftLeaf s
  update _ _ (SV l n m) = SV l (n-1) (m-1)