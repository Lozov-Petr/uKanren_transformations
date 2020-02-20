module Tests.Hanoi where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding
import Util

import Program.Hanoi

---------------------------------------

hanoiDefs :: [Def]
hanoiDefs = fst $ tree2defs hanoi

pins :: Int -> Tx
pins m = C "triple" [gen 0, C "nil" [], C "nil" []] where
  gen n = if n == m then C "nil" [] else C "%" [int2nat n, gen $ n + 1]


hanoiCall :: G X
hanoiCall = Invoke "check" [pins 3, V "answer", C "true" []]

hanoiVars = ["answer"]

hanoiUnit :: RunGoal X ()
hanoiUnit = RG hanoiCall

hanoiInt :: RunGoal X Int
hanoiInt  = RG hanoiCall

hanoiDisj :: RunGoal X (Disj, Int)
hanoiDisj = RG hanoiCall

hanoiEmbed :: RunGoal X Streams
hanoiEmbed = RG hanoiCall

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
  putStrLn $ show $ takeAnswers 1 $ run hanoiVars hanoiDefs () hanoiUnit

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
  putStrLn $ show $ takeAnswers 1 $ run hanoiVars hanoiDefs (sc2 shallowIgnoringEmbed eqAF) hanoiEmbed

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
  putStrLn $ show $ takeAnswers 1 $ run hanoiVars hanoiDefs (sc2 shallowestIgnoringSubformula eqAF) hanoiEmbed
