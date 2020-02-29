module Tests.Bottles where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding
import Util

import Program.Bottles

----------------------------------------------------

bottlesCall = Fresh "c" $
   Invoke "capacities1" [V "c"] :/\:
   Invoke "checkAnswer" [V "answer", V "c", int2nat 7, C "true" []]

bottlesVars = ["answer"]

bottlesUnit :: RunGoal X ()
bottlesUnit = RG bottlesCall

bottlesInt :: RunGoal X Int
bottlesInt = RG bottlesCall

bottlesInv :: RunGoal X Invokes
bottlesInv = RG bottlesCall

bottlesDisj :: RunGoal X (Disj, Int)
bottlesDisj = RG bottlesCall

bottlesEmbed :: RunGoal X Streams
bottlesEmbed = RG bottlesCall

bottlesInvEmbed :: RunGoal X StreamsDict
bottlesInvEmbed = RG bottlesCall

bottlesInvs :: RunGoal X InvokesDict
bottlesInvs = RG bottlesCall

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
  putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles () bottlesUnit

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
  putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (100 :: Int) bottlesInt

----------------------------------------------------

testInvoke50 =
  putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (I 50) bottlesInv

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
  putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (D 10, 10000 :: Int) bottlesDisj

----------------------------------------------------

testShallowestIgnoringEmbed =
  putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (sc2 shallowestIgnoringEmbed eqAF) bottlesEmbed

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
  putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (cmpSD shallowestIgnoringLeftSubformula) bottlesInvEmbed

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
  putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles () bottlesInvs
