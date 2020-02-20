module Tests.Bridge where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding

import Program.Bridge

----------------------------------------------------

bridgeCall =
    Invoke "result" [V "minutes"] &&&
    Invoke "getAnswer" [V "answer", C "some" [V "minutes"]]

bridgeVars = ["minutes", "answer"]

bridgeUnit :: RunGoal X ()
bridgeUnit = RG bridgeCall

bridgeInt :: RunGoal X Int
bridgeInt  = RG bridgeCall

bridgeDisj :: RunGoal X (Disj, Int)
bridgeDisj = RG bridgeCall

bridgeSignVars :: RunGoal X (SignVars, Int)
bridgeSignVars = RG bridgeCall

bridgeEmbed :: RunGoal X Streams
bridgeEmbed = RG bridgeCall

bridgeInvEmbed :: RunGoal X StreamsDict
bridgeInvEmbed = RG bridgeCall

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- first answer
  -- step  :   36060000
  -- path  :         36
  -- height:         51
  -- size  :    2583373
  -- disjs :     545489
  -- conjs :     746197
  -- actCnj:         10
  -- d in c:     262993
  -- maxLs :          0
  -- swaps :          0

testUnit =
  putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big () bridgeUnit

----------------------------------------------------

  -- first answer
  -- step  :    4070000
  -- path  :         17
  -- height:         24
  -- size  :     215985
  -- disjs :      26365
  -- conjs :      81627
  -- actCnj:      13106
  -- d in c:         11
  -- maxLs :          0
  -- swaps :      31813

testInt100 =
  putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (100 :: Int) bridgeInt

----------------------------------------------------

  -- first answer
  -- step  :    1420000
  -- path  :         16
  -- height:         24
  -- size  :      91699
  -- disjs :      13839
  -- conjs :      32010
  -- actCnj:       6504
  -- d in c:         11
  -- maxLs :          0
  -- swaps :       1158

testDisj10 =
  putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (D 10, 10000 :: Int) bridgeDisj

----------------------------------------------------

  -- It doesn't work

testSignVars =
  putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (SVP [0, 1] 100, 10000 :: Int) bridgeSignVars

----------------------------------------------------

  -- first answer
  -- step  :    6250000
  -- path  :         21
  -- height:         27
  -- size  :     639561
  -- disjs :      52941
  -- conjs :     266839
  -- actCnj:      42533
  -- d in c:          2
  -- maxLs :         11
  -- swaps :    1056625

testShallowestIgnoringEmbed =
  putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (sc2 shallowestIgnoringEmbed eqAF) bridgeEmbed

----------------------------------------------------

  -- first answer
  -- step  :    6260000
  -- path  :         14
  -- height:         27
  -- size  :     637875
  -- disjs :      53007
  -- conjs :     265930
  -- actCnj:      41209
  -- d in c:          2
  -- maxLs :         11
  -- swaps :    1055534

testShallowIngoringEmbed =
  putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (sc2 shallowIgnoringEmbed eqAF) bridgeEmbed

----------------------------------------------------

  -- !!! Really slow: deep embedding is quite expensive

testDeepIgnoringEmbed =
  putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (sc2 deepIgnoringEmbed eqAF) bridgeEmbed

----------------------------------------------------

  -- first answer
  -- step  :    4470000
  -- path  :         21
  -- height:         25
  -- size  :     376563
  -- disjs :      31817
  -- conjs :     156464
  -- actCnj:      25733
  -- d in c:          2
  -- maxLs :         11
  -- swaps :     742304

testShallowestIgnoringSubformula =
  putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (sc2 shallowestIgnoringSubformula eqAF) bridgeEmbed

----------------------------------------------------

  -- first answer
  -- step  :    5650000
  -- path  :         18
  -- height:         26
  -- size  :     541041
  -- disjs :      43938
  -- conjs :     226582
  -- actCnj:      36154
  -- d in c:          2
  -- maxLs :         11
  -- swaps :     798299

testShallowestIgnoringLeftSubformula =
  putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (sc2 shallowestIgnoringLeftSubformula eqAF) bridgeEmbed

----------------------------------------------------

  -- first answer
  -- step  :   14990000
  -- path  :         24
  -- height:         32
  -- size  :    2639769
  -- disjs :     270283
  -- conjs :    1049601
  -- actCnj:      81624
  -- d in c:       1089
  -- maxLs :       3763
  -- swaps :     336183

testInvLeftSubformula =
  putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (cmpSD shallowestIgnoringLeftSubformula) bridgeInvEmbed

----------------------------------------------------
  -- first answer
--   step  :   16230000
--   path  :         26
--   height:         31
--   size  :    2427047
--   disjs :     273148
--   conjs :     940375
--   actCnj:      61859
--   d in c:       1307
--   maxLs :       6131
--   swaps :     110939

testInvLeftSubformulaCmpHeights =
  putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (cmpSD cmpHeightsIgnoringLeftSubformula) bridgeInvEmbed
