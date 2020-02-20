module Tests.Sudoku4x4 where

import Syntax

import FairEval
import FairStream
import Labels

import Program.Sudoku4x4

---------------------------------------

sudokuExample q =
  fresh ["v12", "v13", "v14", "v21", "v22", "v23", "v31", "v32", "v34", "v41", "v43", "v44"] $
  V q :=: C "s4x4" [n1,      V "v12", V "v13", V "v14",
                    V "v21", V "v22", V "v23", n4,
                    V "v31", V "v32", n2,      V "v34",
                    V "v41", n3,      V "v43", V "v44"]

sudokuCall :: G X
sudokuCall = sudokuExample "sudoku" :/\: Invoke "check_sudoku" [V "sudoku", C "true" []]

sudokuVars = ["sudoku"]

sudokuUnit :: RunGoal X ()
sudokuUnit = RG sudokuCall

sudokuInt :: RunGoal X Int
sudokuInt  = RG sudokuCall

sudokuDisj :: RunGoal X (Disj, Int)
sudokuDisj = RG sudokuCall

sudokuEmbed :: RunGoal X Streams
sudokuEmbed = RG sudokuCall

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- did not wait for an answer
  -- step  :   22340000
  -- path  :         22
  -- height:         35
  -- size  :     229253
  -- disjs :      46670
  -- conjs :      67956
  -- actCnj:         21
  -- d in c:       8013
  -- maxLs :          0
  -- swaps :          0

testUnit =
  putStrLn $ show $ takeAnswers 1 $ run sudokuVars sudoku4x4 () sudokuUnit

----------------------------------------------------

  -- did not wait for an answer
  -- step  :    9090000
  -- path  :         21
  -- height:         28
  -- size  :    1185209
  -- disjs :      93105
  -- conjs :     499499
  -- actCnj:      32547
  -- d in c:         23
  -- maxLs :          0
  -- swaps :     188944

testInt100 =
  putStrLn $ show $ takeAnswers 1 $ run sudokuVars sudoku4x4 (100 :: Int) sudokuInt

  ----------------------------------------------------

testDisj10 =
  putStrLn $ show $ takeAnswers 1 $ run sudokuVars sudoku4x4 (D 10, 10000 :: Int) sudokuDisj
