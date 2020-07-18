module Tests.BridgeAutogen where

import Syntax
import Util

import Program.BridgeAutogen

import qualified Unfolding as U

defs :: [Def]
defs = fst $ tree2defs $ fst tree (V "x" === V "x")

goal = Invoke "getAnswer" [V "answer", C "some" [int2nat 17]]

vars = ["answer"]

testUnfoldSimpl =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 U.simpleSep vars defs goal
