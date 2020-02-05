module Main where

import Fair
import Tests

import Program.Bottles
import Program.Bridge

main = do
  -- putStrLn $ show $ run 1 0 bottles bottlesUnit
  -- putStrLn $ show $ run 1 0 bottles bottlesInt
  -- putStrLn $ show $ run 1 0 game2Big bridgeUnit
   putStrLn $ show $ run 1 0 game2Big bridgeInt