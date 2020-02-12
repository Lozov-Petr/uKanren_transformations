module Main where

import FairEval
import Tests
import Labels
import Embedding

import Program.Bottles
import Program.Bridge

main = do

  -- putStrLn $ show $ run listAB_vars listAB_def (SC shallowestIgnoringEmbed) listAB_embed
  -- putStrLn $ show $ run listAB_vars listAB_def (SC shallowestEmbed) listAB_embed
  -- putStrLn $ show $ run treeVars treeDefs (SC shallowestIgnoringEmbed) treeEmbed
  -- putStrLn $ show $ run treeVars treeDefs (SC shallowestEmbed) treeEmbed


  -- first answer
  -- step:    2460000
  -- high:         16
  -- size:      88885
  -- disj:      21208
  -- conj:      23234
  -- cnjA:       6310
  -- maxD:          8
  -- putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles () bottlesUnit

  ----------------------------------------------------

  -- first answer
  -- step: 28100000
  -- high: 20
  -- size: 2495765
  -- disj: 309593
  -- conj: 938289
  -- cnjA: 162853
  -- maxD: 15
  -- putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (100 :: Int) bottlesInt

  ----------------------------------------------------

  -- first answer
  -- step: 2460000
  -- high: 15
  -- size: 88883
  -- disj: 21208
  -- conj: 23233
  -- cnjA: 6310
  -- maxD: 8
  -- putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (D 10 10000) bottlesDisj

  -- putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (SC shallowestIgnoringEmbed) bottlesEmbed
  putStrLn $ show $ takeAnswers 1 $ run bottlesVars bottles (SC shallowestEmbed) bottlesEmbed

  ----------------------------------------------------
  ----------------------------------------------------
  ----------------------------------------------------

  -- OUT OF MEMORY
  -- step: 8530000
  -- high: 30
  -- size: 460289
  -- disj: 99876
  -- conj: 130268
  -- cnjA: 9
  -- maxD: 48017
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big () bridgeUnit

  ----------------------------------------------------

  -- first answer
  -- step: 3630000
  -- high: 15
  -- size: 172321
  -- disj: 18419
  -- conj: 67741
  -- cnjA: 9756
  -- maxD: 12
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (100 :: Int) bridgeInt

  ----------------------------------------------------

  -- fitst answer
  -- step: 2260000
  -- high: 19
  -- size: 103847
  -- disj: 14841
  -- conj: 37082
  -- cnjA: 7389
  -- maxD: 10
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (D 10 10000) bridgeDisj

  ----------------------------------------------------
  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (SVP [0, 1] 100 10000) bridgeVars

  -- putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (SC shallowestIgnoringEmbed) bridgeEmbed
  putStrLn $ show $ takeAnswers 1 $ run bridgeVars game2Big (SC shallowestEmbed) bridgeEmbed