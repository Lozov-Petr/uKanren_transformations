module Main where

import FairEval
import Tests
import Labels
import Embedding

import Program.Bottles
import Program.Bridge

----------------------------------------------------

main = do

  putStrLn $ show $ run listAB_vars listAB_def (sc1 shallowestIgnoringEmbed) listAB_embed
  putStrLn $ show $ run listAB_vars listAB_def (sc2 shallowestIgnoringEmbed eqAF) listAB_embed
  putStrLn $ show $ run listAB_vars listAB_def (sc1 shallowestEmbed) listAB_embed
  putStrLn $ show $ run treeVars treeDefs (sc1 shallowestIgnoringEmbed) treeEmbed
  putStrLn $ show $ run treeVars treeDefs (sc2 shallowestIgnoringEmbed eqAF) treeEmbed
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
  -- putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (D 10 10000) bottlesDisj

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
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (D 10 10000) bridgeDisj

  ----------------------------------------------------

  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (SVP [0, 1] 100 10000) bridgeVars

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

  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (sc1 shallowestIgnoringEmbed) bridgeEmbed
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (sc1 shallowestEmbed) bridgeEmbed