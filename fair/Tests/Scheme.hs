module Tests.Scheme where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding
import Util

import Program.Scheme

---------------------------------------

schemeDefs :: [Def]
schemeDefs = fst $ tree2defs scheme

schemeCall :: G X
schemeCall = Invoke "eval" [V "quine", C "nil" [], C "val" [V "quine"]]

schemeVars = ["quine"]

schemeUnit :: RunGoal X ()
schemeUnit = RG schemeCall

schemeInt :: RunGoal X Int
schemeInt  = RG schemeCall

schemeDisj :: RunGoal X (Disj, Int)
schemeDisj = RG schemeCall

schemeEmbed :: RunGoal X Streams
schemeEmbed = RG schemeCall

schemeInvEmbed :: RunGoal X StreamsDict
schemeInvEmbed = RG schemeCall

schemeInvs :: RunGoal X InvokesDict
schemeInvs = RG schemeCall

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- first answer
  -- step  :     360000
  -- path  :          9
  -- height:         37
  -- size  :      63971
  -- disjs :      17902
  -- conjs :      14083
  -- actCnj:        924
  -- d in c:        418
  -- maxLs :          0
  -- swaps :          0

testUnit =
  putStrLn $ show $ takeAnswers 1 $ run schemeVars schemeDefs () schemeUnit

----------------------------------------------------

  -- first answer
  -- step  :     960000
  -- path  :         20
  -- height:         26
  -- size  :     711263
  -- disjs :      82124
  -- conjs :     273507
  -- actCnj:      30204
  -- d in c:         23
  -- maxLs :          0
  -- swaps :      10267

testInt100 =
  putStrLn $ show $ takeAnswers 1 $ run schemeVars schemeDefs (100 :: Int) schemeInt

----------------------------------------------------

  -- first answer
  -- step  :     760000
  -- path  :          9
  -- height:         28
  -- size  :     415621
  -- disjs :      56611
  -- conjs :     151199
  -- actCnj:      21105
  -- d in c:         11
  -- maxLs :          0
  -- swaps :       2597

testDisj10 =
  putStrLn $ show $ takeAnswers 1 $ run schemeVars schemeDefs (D 10, 10000 :: Int) schemeDisj

----------------------------------------------------

  -- first answer
  -- step  :    3010000
  -- path  :         23
  -- height:         30
  -- size  :    3947961
  -- disjs :     202049
  -- conjs :    1771931
  -- actCnj:     166139
  -- d in c:          2
  -- maxLs :         18
  -- swaps :     454372

testShallowestIgnoringSubformula =
  putStrLn $ show $ takeAnswers 1 $ run schemeVars schemeDefs (sc2 shallowestIgnoringSubformula eqAF) schemeEmbed

----------------------------------------------------

  -- first answer
  -- step  :    1600000
  -- path  :         19
  -- height:         28
  -- size  :    1719163
  -- disjs :     122219
  -- conjs :     737362
  -- actCnj:      98101
  -- d in c:          2
  -- maxLs :         13
  -- swaps :     151162

testShallowestIgnoringLeftSubformula =
  putStrLn $ show $ takeAnswers 1 $ run schemeVars schemeDefs (sc2 shallowestIgnoringLeftSubformula eqAF) schemeEmbed

----------------------------------------------------

  -- first answer
  -- step  :    2480000
  -- path  :         20
  -- height:         27
  -- size  :    2026917
  -- disjs :     250059
  -- conjs :     763399
  -- actCnj:      49286
  -- d in c:       1584
  -- maxLs :       1450
  -- swaps :      34631

testInvLeftSubformula =
  putStrLn $ show $ takeAnswers 1 $ run schemeVars schemeDefs (cmpSD shallowestIgnoringLeftSubformula) schemeInvEmbed

----------------------------------------------------

  -- first answer
  -- step  :     460000
  -- path  :         19
  -- height:         26
  -- size  :     330525
  -- disjs :      47550
  -- conjs :     117712
  -- actCnj:       9547
  -- d in c:        814
  -- maxLs :        704
  -- swaps :       3859

testInvLeftSubformulaCmpHeights =
  putStrLn $ show $ takeAnswers 1 $ run schemeVars schemeDefs (cmpSD cmpHeightsIgnoringLeftSubformula) schemeInvEmbed

----------------------------------------------------

  -- first answer
  -- step  :     940000
  -- path  :         21
  -- height:         25
  -- size  :     748135
  -- disjs :      79469
  -- conjs :     294598
  -- actCnj:      37188
  -- d in c:         17
  -- maxLs :         23
  -- swaps :      31179

testInvsSubinvoke =
  putStrLn $ show $ takeAnswers 1 $ run schemeVars schemeDefs () schemeInvs
