module Main where

import qualified Tests.ListAB    as ListAB
import qualified Tests.Trees     as Trees
-- import qualified Tests.Bottles   as Bottles
-- import qualified Tests.Bridge    as Bridge
-- import qualified Tests.GCW       as GCW
-- import qualified Tests.Hanoi     as Hanoi
-- import qualified Tests.Scheme    as Scheme
-- import qualified Tests.Sudoku4x4 as Sudoku4x4
-- import qualified Tests.Reverso   as Reverso

----------------------------------------------------

main = do
  ListAB.tests
  Trees.tests

  -- Bridge.testDisj10
  -- Bridge.testInvLeftSubformula
  -- Bridge.testInvLeftSubformulaCmpHeights

  -- Hanoi.testInvLeftSubformula
  -- Hanoi.testInvLeftSubformulaCmpHeights

  -- Scheme.testInvLeftSubformula
  -- Scheme.testInvLeftSubformulaCmpHeights

  -- Reverso.testUnit1 50
