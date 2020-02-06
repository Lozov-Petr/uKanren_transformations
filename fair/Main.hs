module Main where

import Fair
import Tests

import Program.Bottles
import Program.Bridge

main = do
  -- putStrLn $ show $ run 1 0 bottles bottlesUnit  -- get first answer ( 2460000, 15,  21208,   88883)
  -- putStrLn $ show $ run 1 0 bottles bottlesInt   -- looooong run     (15660000, 18, 157219, 1149109)
  -- putStrLn $ show $ run 1 0 bottles bottlesDisj  -- same with Unit   ( 2460000, 15,  21208,   88883)
  -- putStrLn $ show $ run 1 0 game2Big bridgeUnit  -- out of memory    ( 3260000, 28,  38770,  180845)
     putStrLn $ show $ run 1 0 game2Big bridgeInt   -- get first answer ( 3630000, 15,  18419,  172321)
  -- putStrLn $ show $ run 1 0 game2Big bridgeDisj  -- looooong run     (27920000, 21,  23584, 2046769)