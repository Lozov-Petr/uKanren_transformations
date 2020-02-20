module Tests.GCW where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding
import Util

import Program.GCW

---------------------------------------

gcwDefs :: [Def]
gcwDefs = fst $ tree2defs gcw

gcwCall :: G X
gcwCall = Invoke "checkAnswer" [V "answer", C "true" []]

gcwVars = ["answer"]

gcwUnit :: RunGoal X ()
gcwUnit = RG gcwCall

gcwInt :: RunGoal X Int
gcwInt  = RG gcwCall

gcwDisj :: RunGoal X (Disj, Int)
gcwDisj = RG gcwCall

gcwEmbed :: RunGoal X Streams
gcwEmbed = RG gcwCall

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- 100 answers
  -- step  :    1310000
  -- path  :         15
  -- height:         19
  -- size  :      74483
  -- disjs :      20821
  -- conjs :      16420
  -- actCnj:       9348
  -- d in c:          5
  -- maxLs :          0
  -- swaps :          0

testUnit =
  putStrLn $ show $ takeAnswers 100 $ run gcwVars gcwDefs () gcwUnit

----------------------------------------------------

testShallowesIgnoringSubformula =
  putStrLn $ show $ takeAnswers 100 $ run gcwVars gcwDefs (sc2 shallowestIgnoringSubformula eqAF) gcwEmbed
