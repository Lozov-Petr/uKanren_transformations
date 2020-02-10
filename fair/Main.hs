module Main where

import Fair
import Tests

import Program.Bottles
import Program.Bridge

main = do

  -- first answer
  -- step:    2460000
  -- high:         16
  -- size:      88885
  -- disj:      21208
  -- conj:      23234
  -- cnjA:       6310
  -- maxD:          8
  -- putStrLn $ show $ run 1 [0] bottles () bottlesUnit

  ----------------------------------------------------

  -- first answer
  -- step: 28100000
  -- high: 20
  -- size: 2495765
  -- disj: 309593
  -- conj: 938289
  -- cnjA: 162853
  -- maxD: 15
  -- putStrLn $ show $ run 1 [0] bottles (100 :: Int) bottlesInt

  ----------------------------------------------------

  -- first answer
  -- step: 2460000
  -- high: 15
  -- size: 88883
  -- disj: 21208
  -- conj: 23233
  -- cnjA: 6310
  -- maxD: 8
  -- putStrLn $ show $ run 1 [0] bottles (D 10 10000) bottlesDisj

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
  -- putStrLn $ show $ run 1 [1, 0] game2Big () bridgeUnit

  ----------------------------------------------------

  -- first answer
  -- step: 3630000
  -- high: 15
  -- size: 172321
  -- disj: 18419
  -- conj: 67741
  -- cnjA: 9756
  -- maxD: 12
  -- putStrLn $ show $ run 1 [1, 0] game2Big (100 :: Int) bridgeInt

  ----------------------------------------------------

  -- fitst answer
  -- step: 2260000
  -- high: 19
  -- size: 103847
  -- disj: 14841
  -- conj: 37082
  -- cnjA: 7389
  -- maxD: 10
  putStrLn $ show $ run 1 [1, 0] game2Big (D 10 10000) bridgeDisj

  ----------------------------------------------------
  -- putStrLn $ show $ run 1 [1, 0] game2Big (SVP [0, 1] 100 10000) bridgeVars