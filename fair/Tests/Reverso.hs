module Tests.Reverso where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding

import Program.List

----------------------------------------------------

def2 :: Def
def2 = Def "reverso" ["x", "y"] $
  (V "x" === nil &&& V "y" === nil) |||
  fresh ["e", "xs", "ys"] (
    V "x" === V "e" % V "xs" &&&
    call "appendo" [V "ys", V "e" % nil, V "y"] &&&
    call "reverso" [V "xs", V "ys"])

defs1 :: [Def]
defs1 = reverso

defs2 :: [Def]
defs2 = def2 : appendo

genList :: Int -> Tx
genList 0 = nil
genList n = C (show $ n - 1) [] % genList (n - 1)

call1 :: Int -> G X
call1 n = call "reverso" [genList n, V "answer"]

call2 :: Int -> G X
call2 n = call "reverso" [V "answer", genList n]

vars = ["answer"]

call1Unit :: Int -> RunGoal X ()
call1Unit = RG . call1

call2Unit :: Int -> RunGoal X ()
call2Unit = RG . call2

call1Int :: Int -> RunGoal X Int
call1Int  = RG . call1

call2Int :: Int -> RunGoal X Int
call2Int  = RG . call2

call1Disj :: Int -> RunGoal X (Disj, Int)
call1Disj = RG . call1

call2Disj :: Int -> RunGoal X (Disj, Int)
call2Disj = RG . call2

call1Inv :: Int -> RunGoal X Invokes
call1Inv  = RG . call1

call2Inv :: Int -> RunGoal X Invokes
call2Inv  = RG . call2

call1Embed :: Int -> RunGoal X Streams
call1Embed = RG . call1

call2Embed :: Int -> RunGoal X Streams
call2Embed = RG . call2

call1InvEmbed :: Int -> RunGoal X StreamsDict
call1InvEmbed = RG . call1

call2InvEmbed :: Int -> RunGoal X StreamsDict
call2InvEmbed = RG . call2

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- first answer, length: 100
  -- step  :      40000
  -- path  :          1
  -- height:          1
  -- size  :          3
  -- disjs :          0
  -- conjs :          1
  -- actCnj:          1
  -- d in c:          0
  -- maxLs :          0
  -- swaps :          0

testUnit1_1 =
  putStrLn . show . run vars defs1 () . call1Unit

----------------------------------------------------

  -- all answers, length: 100
  -- step  :    1400000
  -- path  :         74
  -- height:        126
  -- size  :        281
  -- disjs :         16
  -- conjs :        124
  -- actCnj:          1
  -- d in c:         16
  -- maxLs :          0
  -- swaps :          0

testUnit1_2 =
  putStrLn . show . takeAnswers 1 . run vars defs1 () . call2Unit

----------------------------------------------------

  -- first answer, length: 100
  -- step  :    1620000
  -- path  :          4
  -- height:         15
  -- size  :         95
  -- disjs :         22
  -- conjs :         25
  -- actCnj:         14
  -- d in c:          1
  -- maxLs :          0
  -- swaps :          0

testUnit2_1 =
  putStrLn . show . takeAnswers 1 . run vars defs2 () . call1Unit

----------------------------------------------------

  -- all answers, length: 100
  -- step  :      40000
  -- path  :          3
  -- height:          3
  -- size  :          7
  -- disjs :          1
  -- conjs :          2
  -- actCnj:          1
  -- d in c:          1
  -- maxLs :          0
  -- swaps :          0

testUnit2_2 =
  putStrLn . show . run vars defs2 () . call2Unit

----------------------------------------------------

  -- first answer, length: 100
  -- step  :    4920000
  -- path  :          5
  -- height:         34
  -- size  :       3075
  -- disjs :        100
  -- conjs :       1437
  -- actCnj:         68
  -- d in c:          1
  -- maxLs :          0
  -- swaps :      72779

testInt1_1 =
  putStrLn . show . takeAnswers 1 . run vars defs1 (100 :: Int) . call1Int

----------------------------------------------------
----------------------------------------------------

  -- all answers, length: 100
  -- step  :    2170000
  -- path  :          4
  -- height:         21
  -- size  :        239
  -- disjs :         13
  -- conjs :        106
  -- actCnj:          7
  -- d in c:          3
  -- maxLs :          0
  -- swaps :      31822

testInt1_2 =
  putStrLn . show . run vars defs1 (100 :: Int) . call2Int

----------------------------------------------------

  -- first answer, length: 100
  -- step  :    4630000
  -- path  :          4
  -- height:         19
  -- size  :        833
  -- disjs :         83
  -- conjs :        333
  -- actCnj:         56
  -- d in c:          3
  -- maxLs :          0
  -- swaps :      59359

testInt2_1 =
  putStrLn . show . takeAnswers 1 . run vars defs2 (100 :: Int) . call1Int

----------------------------------------------------

  -- all answers, length: 100
  -- step  :     140000
  -- path  :          3
  -- height:          4
  -- size  :         17
  -- disjs :          3
  -- conjs :          5
  -- actCnj:          2
  -- d in c:          1
  -- maxLs :          0
  -- swaps :       1911

testInt2_2 =
  putStrLn . show . run vars defs2 (100 :: Int) . call2Int

----------------------------------------------------

  -- all answers, length: 100
  -- step  :    2770000
  -- path  :          4
  -- height:         66
  -- size  :       2457
  -- disjs :         44
  -- conjs :       1184
  -- actCnj:         26
  -- d in c:          1
  -- maxLs :          0
  -- swaps :       3822

testDisj1_1 =
  putStrLn . show . run vars defs1 (D 10, 10000 :: Int) . call1Disj

----------------------------------------------------
----------------------------------------------------

  -- first answer, length: 100
  -- step  :    1560000
  -- path  :          4
  -- height:        103
  -- size  :       2415
  -- disjs :         28
  -- conjs :       1179
  -- actCnj:         16
  -- d in c:          1
  -- maxLs :          0
  -- swaps :       1061

testDisj1_2 =
  putStrLn . show . takeAnswers 1 . run vars defs1 (D 10, 10000 :: Int) . call2Disj

----------------------------------------------------

  -- first answer, length: 100
  -- step  :    1620000
  -- path  :          4
  -- height:         15
  -- size  :         95
  -- disjs :         22
  -- conjs :         25
  -- actCnj:         14
  -- d in c:          1
  -- maxLs :          0
  -- swaps :          0

testDisj2_1 =
  putStrLn . show . takeAnswers 1 . run vars defs2 (D 10, 10000 :: Int) . call1Disj


----------------------------------------------------

  -- all answers, length: 100
  -- step  :      40000
  -- path  :          3
  -- height:          3
  -- size  :          7
  -- disjs :          1
  -- conjs :          2
  -- actCnj:          1
  -- d in c:          1
  -- maxLs :          0
  -- swaps :          0

testDisj2_2 =
  putStrLn . show . run vars defs2 (D 10, 10000 :: Int) . call2Disj

----------------------------------------------------
----------------------------------------------------

  -- first answer, length: 100
  -- step  :    1700000
  -- path  :          7
  -- height:         22
  -- size  :        303
  -- disjs :         24
  -- conjs :        127
  -- actCnj:         15
  -- d in c:          1
  -- maxLs :          0
  -- swaps :      71795

testInv1_1 =
  putStrLn . show . takeAnswers 1 . run vars defs1 (I 5) . call1Inv

----------------------------------------------------

  -- all answers, length: 100
  -- step  :     110000
  -- path  :          3
  -- height:          7
  -- size  :         35
  -- disjs :          4
  -- conjs :         13
  -- actCnj:          3
  -- d in c:          1
  -- maxLs :          0
  -- swaps :       4393

testInv1_2 =
  putStrLn . show . run vars defs1 (I 5) . call2Inv

----------------------------------------------------

  -- first answer, length: 100
  -- step  :    1690000
  -- path  :          6
  -- height:         18
  -- size  :        189
  -- disjs :         21
  -- conjs :         73
  -- actCnj:         13
  -- d in c:          1
  -- maxLs :          0
  -- swaps :      66508

testInv2_1 =
  putStrLn . show . takeAnswers 1 . run vars defs2 (I 5) . call1Inv

----------------------------------------------------

-- all answers, length: 100
-- step  :      50000
-- path  :          3
-- height:          3
-- size  :         11
-- disjs :          2
-- conjs :          3
-- actCnj:          2
-- d in c:          1
-- maxLs :          0
-- swaps :       2013

testInv2_2 =
  putStrLn . show . run vars defs2 (I 5) . call1Inv

----------------------------------------------------
----------------------------------------------------

  -- did not wait for an answer
  -- step  :     360000
  -- path  :         16
  -- height:         69
  -- size  :       1287
  -- disjs :         16
  -- conjs :        627
  -- actCnj:         13
  -- d in c:          1
  -- maxLs :         39
  -- swaps :       1645

testInvLeftSubformulaCmpHeights1_1 =
  putStrLn . show . takeAnswers 1 . run vars defs1 (cmpSD cmpHeightsIgnoringLeftSubformula) . call1InvEmbed

  ----------------------------------------------------

testInvLeftSubformulaCmpHeights1_2 =
  putStrLn . show . takeAnswers 1 . run vars defs1 (cmpSD cmpHeightsIgnoringLeftSubformula) . call2InvEmbed

----------------------------------------------------

testInvLeftSubformulaCmpHeights2_1 =
  putStrLn . show . takeAnswers 1 . run vars defs2 (cmpSD cmpHeightsIgnoringLeftSubformula) . call1InvEmbed

----------------------------------------------------

testInvLeftSubformulaCmpHeights2_2 =
  putStrLn . show . takeAnswers 1 . run vars defs2 (cmpSD cmpHeightsIgnoringLeftSubformula) . call2InvEmbed
