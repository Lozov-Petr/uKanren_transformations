module Tests.Bottles where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding
import Util

import Program.Bottles

----------------------------------------------------

defs :: [Def]
defs = bottles

goal = Fresh "c" $
   Invoke "capacities1" [V "c"] :/\:
   Invoke "checkAnswer" [V "answer", V "c", int2nat 7, C "true" []]

vars = ["answer"]

goalUnit :: RunGoal X ()
goalUnit = RG goal

goalInt :: RunGoal X Int
goalInt = RG goal

goalInv :: RunGoal X Invokes
goalInv = RG goal

goalDisj :: RunGoal X (Disj, Int)
goalDisj = RG goal

goalEmbed :: RunGoal X Streams
goalEmbed = RG goal

goalInvEmbed :: RunGoal X StreamsDict
goalInvEmbed = RG goal

goalInvs :: RunGoal X InvokesDict
goalInvs = RG goal

goalDefs :: RunGoal X DefsLabel
goalDefs = RG goal

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- first answer
  -- step  :    2080000
  -- path  :         15
  -- height:         22
  -- size  :      96111
  -- disjs :      22250
  -- conjs :      25805
  -- actCnj:       6397
  -- d in c:          9
  -- maxLs :          0
  -- swaps :          0

testUnit =
  putStrLn $ show $ takeAnswers 1 $ run vars defs () goalUnit

----------------------------------------------------

  -- first answer
  -- step  :   23920000
  -- path  :         17
  -- height:         27
  -- size  :    2185409
  -- disjs :     290402
  -- conjs :     802302
  -- actCnj:     145327
  -- d in c:         15
  -- maxLs :          0
  -- swaps :     198131

testInt100 =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (100 :: Int) goalInt

----------------------------------------------------

testInvoke50 =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (I 50) goalInv

----------------------------------------------------

  -- first answer
  -- step  :    2080000
  -- path  :         15
  -- height:         22
  -- size  :      96111
  -- disjs :      22250
  -- conjs :      25805
  -- actCnj:       6397
  -- d in c:          9
  -- maxLs :          0
  -- swaps :          0

testDisj10 =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (D 10, 10000 :: Int) goalDisj

----------------------------------------------------

testShallowestIgnoringEmbed =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (sc2 shallowestIgnoringEmbed eqAF) goalEmbed

----------------------------------------------------

  -- did not wait for an answer
  -- step  :   19110000
  -- path  :         17
  -- height:         31
  -- size  :    2324083
  -- disjs :     306058
  -- conjs :     855983
  -- actCnj:      92056
  -- d in c:       1238
  -- maxLs :       6841
  -- swaps :     599778

testInvokeSubformula =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (cmpSD shallowestIgnoringLeftSubformula) goalInvEmbed

----------------------------------------------------

  -- first answer
  -- step  :    4300000
  -- path  :         15
  -- height:         24
  -- size  :     319749
  -- disjs :      49650
  -- conjs :     110224
  -- actCnj:      23235
  -- d in c:          9
  -- maxLs :         22
  -- swaps :      32551

testInvsSubinvoke =
  putStrLn $ show $ takeAnswers 1 $ run vars defs () goalInvs

----------------------------------------------------

  -- did not wait for an answer
  -- step  :   25010000
  -- path  :         18
  -- height:         29
  -- size  :    1469969
  -- disjs :     234174
  -- conjs :     500810
  -- actCnj:      75208
  -- d in c:         17
  -- maxLs :          0
  -- swaps :     452296

testDefsApprox =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (toDA defs) goalDefs
