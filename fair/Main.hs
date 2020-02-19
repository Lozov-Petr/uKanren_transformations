module Main where

import FairEval
import Tests
import Labels
import Embedding

import Program.Bottles
import Program.Bridge
import Program.Sudoku4x4

----------------------------------------------------

main = do

  putStrLn $ show $ run listAB_vars listAB_def (I 5) listAB_inv
  putStrLn $ show $ run listAB_vars listAB_def (sc1 shallowestIgnoringEmbed) listAB_embed
  putStrLn $ show $ run listAB_vars listAB_def (sc2 shallowestIgnoringEmbed eqAF) listAB_embed
  putStrLn $ show $ run listAB_vars listAB_def (sc2 shallowIgnoringEmbed eqAF) listAB_embed
  putStrLn $ show $ run listAB_vars listAB_def (sc2 deepIgnoringEmbed eqAF) listAB_embed
  putStrLn $ show $ run listAB_vars listAB_def (sc2 shallowestIgnoringSubformula eqAF) listAB_embed
  putStrLn $ show $ run listAB_vars listAB_def (sc1 shallowestEmbed) listAB_embed

  putStrLn $ show $ run treeVars treeDefs (I 5) treeInv
  putStrLn $ show $ run treeVars treeDefs (sc1 shallowestIgnoringEmbed) treeEmbed
  putStrLn $ show $ run treeVars treeDefs (sc2 shallowestIgnoringEmbed eqAF) treeEmbed
  putStrLn $ show $ run treeVars treeDefs (sc2 shallowIgnoringEmbed eqAF) treeEmbed
  putStrLn $ show $ run treeVars treeDefs (sc2 deepIgnoringEmbed eqAF) treeEmbed
  putStrLn $ show $ run treeVars treeDefs (sc2 shallowestIgnoringSubformula eqAF) treeEmbed
  putStrLn $ show $ run treeVars treeDefs (sc1 shallowestEmbed) treeEmbed

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
  -- putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles () bottlesUnit

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
  -- putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (100 :: Int) bottlesInt

  ----------------------------------------------------
  -- putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (I 50) bottlesInv
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
  -- putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (D 10, 10000 :: Int) bottlesDisj

  -- putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (sc1 shallowestIgnoringEmbed) bottlesEmbed
  -- putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (sc2 shallowestIgnoringEmbed eqAF) bottlesEmbed
  -- putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (sc1 shallowestEmbed) bottlesEmbed

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
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big () bridgeUnit

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
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (100 :: Int) bridgeInt

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
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (D 10, 10000 :: Int) bridgeDisj

  ----------------------------------------------------

  -- Doesn't work
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (SVP [0, 1] 100, 10000 :: Int) bridgeSignVars

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
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (sc2 shallowestIgnoringEmbed eqAF) bridgeEmbed

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
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (sc2 shallowIgnoringEmbed eqAF) bridgeEmbed

  ----------------------------------------------------

  -- !!! Really slow: deep embedding is quite expensive
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (sc2 deepIgnoringEmbed eqAF) bridgeEmbed

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
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (sc2 shallowestIgnoringSubformula eqAF) bridgeEmbed

  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (sc1 shallowestIgnoringEmbed) bridgeEmbed
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (sc1 shallowestEmbed) bridgeEmbed

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
  -- putStrLn $ show $ takeAnswers 100 $ run gcwVars gcwDefs () gcwUnit

  ----------------------------------------------------

  -- putStrLn $ show $ takeAnswers 100 $ run gcwVars gcwDefs (sc2 shallowestIgnoringSubformula eqAF) gcwEmbed

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
  -- putStrLn $ show $ takeAnswers 1 $ run hanoiVars hanoiDefs () hanoiUnit

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
  -- putStrLn $ show $ takeAnswers 1 $ run hanoiVars hanoiDefs (sc2 shallowestIgnoringSubformula eqAF) hanoiEmbed

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
  -- putStrLn $ show $ takeAnswers 1 $ run hanoiVars hanoiDefs (sc2 shallowIgnoringEmbed eqAF) hanoiEmbed

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
  -- putStrLn $ show $ takeAnswers 1 $ run schemeVars schemeDefs () schemeUnit

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
  -- putStrLn $ show $ takeAnswers 1 $ run schemeVars schemeDefs (100 :: Int) schemeInt

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
  -- putStrLn $ show $ takeAnswers 1 $ run schemeVars schemeDefs (D 10, 10000 :: Int) schemeDisj

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
  -- putStrLn $ show $ takeAnswers 1 $ run schemeVars schemeDefs (sc2 shallowestIgnoringSubformula eqAF) schemeEmbed

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
  -- putStrLn $ show $ takeAnswers 1 $ run sudokuVars sudoku4x4 () sudokuUnit

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
  --putStrLn $ show $ takeAnswers 1 $ run sudokuVars sudoku4x4 (100 :: Int) sudokuInt

  ----------------------------------------------------

  -- putStrLn $ show $ takeAnswers 1 $ run sudokuVars sudoku4x4 (D 10, 10000 :: Int) sudokuDisj
