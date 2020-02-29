module Tests.Hanoi where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding
import Util

import Program.Hanoi

---------------------------------------

defs :: [Def]
defs = fst $ tree2defs hanoi

pins :: Int -> Tx
pins m = C "triple" [gen 0, C "nil" [], C "nil" []] where
  gen n = if n == m then C "nil" [] else C "%" [int2nat n, gen $ n + 1]


goal :: G X
goal = Invoke "check" [pins 3, V "answer", C "true" []]

vars = ["answer"]

goalUnit :: RunGoal X ()
goalUnit = RG goal

goalInt :: RunGoal X Int
goalInt  = RG goal

goalDisj :: RunGoal X (Disj, Int)
goalDisj = RG goal

goalEmbed :: RunGoal X Streams
goalEmbed = RG goal

goalInvEmbed :: RunGoal X StreamsDict
goalInvEmbed = RG goal

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- first answer
  -- step  :     270000
  -- path  :         15
  -- height:         19
  -- size  :      25071
  -- disjs :       6870
  -- conjs :       5665
  -- actCnj:       1584
  -- d in c:         11
  -- maxLs :          0
  -- swaps :          0

testUnit =
  putStrLn $ show $ takeAnswers 1 $ run vars defs () goalUnit

----------------------------------------------------

  -- first answer
  -- step  :    1140000
  -- path  :         16
  -- height:         21
  -- size  :     209051
  -- disjs :      24850
  -- conjs :      79675
  -- actCnj:      20555
  -- d in c:          2
  -- maxLs :         14
  -- swaps :     107356
testShallowIgnoringEmbed =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (sc2 shallowIgnoringEmbed eqAF) goalEmbed

----------------------------------------------------

  -- first answer
  -- step  :     970000
  -- path  :         17
  -- height:         21
  -- size  :     195089
  -- disjs :      22367
  -- conjs :      75177
  -- actCnj:      18359
  -- d in c:          2
  -- maxLs :         14
  -- swaps :      89731

testShallowestIgnoringSubformula =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (sc2 shallowestIgnoringSubformula eqAF) goalEmbed

----------------------------------------------------

  -- first answer
  -- step  :     590000
  -- path  :         14
  -- height:         23
  -- size  :      58553
  -- disjs :      15794
  -- conjs :      13482
  -- actCnj:       3087
  -- d in c:         17
  -- maxLs :         26
  -- swaps :        596
testInvLeftSubformula =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (cmpSD shallowestIgnoringSubformula) goalInvEmbed

----------------------------------------------------

  -- first answer
  -- step  :     270000
  -- path  :         15
  -- height:         19
  -- size  :      25071
  -- disjs :       6870
  -- conjs :       5665
  -- actCnj:       1584
  -- d in c:         11
  -- maxLs :         28
  -- swaps :          0

testInvLeftSubformulaCmpHeights =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (cmpSD cmpHeightsIgnoringLeftSubformula) goalInvEmbed