{-# LANGUAGE MultiParamTypeClasses #-}

module FairStream where

import Text.Printf

import Syntax

type Er a = Either String a

type Goal  = G S
type Subst = (S, [(S, Ts)])
type Hole  = ()
type Fun   = (Name, ([Name], G X))

data GenStream subst label
  = Goal Goal subst
  | Disj (GenStream subst label) (GenStream subst label)
  | Conj (GenStream subst label) label (GenStream Hole label)

type HoleStream l = GenStream Hole l
type Stream     l = GenStream Subst l

newtype RunGoal x l = RG (G x)

instance (Show s, Show l) => Show (GenStream s l) where
  show (Goal g s) = printf "<%s, %s>" (show g) $ show s
  show (Disj p q)   = printf "(%s |+| %s)" (show p) $ show q
  show (Conj s l g) = printf "(%s |*|{%s} %s)" (show s) (show l) $ show g

---------------------------------------

class Labels l p where
  new       :: p -> Stream l -> l
  keep      :: p -> l -> l
  predicate :: p -> Stream l -> l -> Bool
  update    :: p -> Stream l -> l -> l