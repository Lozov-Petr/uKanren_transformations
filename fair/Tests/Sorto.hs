module Tests.Sorto where

import Syntax
import Program.Sort
import Program.List

import FairStream
import FairEval
import Labels

defs1 :: [Def]
defs1 = sorto

defs2 :: [Def]
defs2 = sorto'

genNat :: Int -> Tx
genNat 0 = C "O" []
genNat n = C "S" [genNat $ n - 1]

genList :: Int -> Tx
genList 0 = nil
genList n = genNat n % genList (n - 1)

goal :: Int -> G X
goal x = call "sorto" [genList x, V "x"]

vars = ["x"]

goalUnit :: Int -> RunGoal X ()
goalUnit = RG . goal

goalInt :: Int -> RunGoal X Int
goalInt  = RG . goal

goalDisj :: Int -> RunGoal X (Disj, Int)
goalDisj = RG . goal

goalInv :: Int -> RunGoal X Invokes
goalInv  = RG . goal

goalEmbed :: Int -> RunGoal X Streams
goalEmbed = RG . goal

goalInvEmbed :: Int -> RunGoal X StreamsDict
goalInvEmbed = RG . goal

goalInvs :: Int -> RunGoal X InvokesDict
goalInvs = RG . goal

goalDefs :: Int -> RunGoal X DefsLabel
goalDefs = RG . goal

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- did not wait for an answer (stack owerflow space), length: 30
  -- step  :    4460000
  -- path  :          4
  -- height:         33
  -- size  :        567
  -- disjs :        164
  -- conjs :        119
  -- actCnj:          5
  -- d in c:        155
  -- maxLs :          0
  -- swaps :          0

testUnit1 = putStrLn . show . takeAnswers 1 . run vars defs1 () . goalUnit

----------------------------------------------------

  -- all  answers, length: 30
  -- step  :    1840000
  -- path  :          5
  -- height:          8
  -- size  :         53
  -- disjs :         12
  -- conjs :         14
  -- actCnj:          2
  -- d in c:          7
  -- maxLs :          0
  -- swaps :          0

testUnit2 = putStrLn . show . run vars defs2 () . goalUnit

----------------------------------------------------

  -- all  answers, length: 30
  -- step  :    7070000
  -- path  :          5
  -- height:          9
  -- size  :         61
  -- disjs :         13
  -- conjs :         17
  -- actCnj:          1
  -- d in c:         13
  -- maxLs :          0
  -- swaps :       4045

testDefsApprox1 = putStrLn . show . run vars defs1 (toDA defs1) . goalDefs

----------------------------------------------------

  -- all  answers, length: 30
  -- step  :    4600000
  -- path  :          5
  -- height:          6
  -- size  :         23
  -- disjs :          4
  -- conjs :          7
  -- actCnj:          1
  -- d in c:          4
  -- maxLs :          0
  -- swaps :       3203

testDefsApprox2 = putStrLn . show . run vars defs2 (toDA defs2) . goalDefs
