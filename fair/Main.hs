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

  -- Bottles.testInvsSubinvoke
  -- Bottles.testDefsApprox

  -- Bridge.testDisj10
  -- Bridge.testInvLeftSubformula
  -- Bridge.testInvLeftSubformulaCmpHeights
  -- Bridge.testInvsSubinvoke
  -- Bridge.testDefsApprox

  -- Hanoi.testInvLeftSubformula
  -- Hanoi.testInvLeftSubformulaCmpHeights

  -- Scheme.testInvLeftSubformula
  -- Scheme.testInvLeftSubformulaCmpHeights
  -- Scheme.testInvsSubinvoke

  -- Reverso.testUnit1_1 100
  -- Reverso.testUnit1_2 100
  -- Reverso.testUnit2_1 100
  -- Reverso.testUnit2_2 100
  -- Reverso.testInt1_1 100
  -- Reverso.testInt1_2 100
  -- Reverso.testInt2_1 100
  -- Reverso.testInt2_2 100
  -- Reverso.testDisj1_1 100
  -- Reverso.testDisj1_2 100
  -- Reverso.testDisj2_1 100
  -- Reverso.testDisj2_2 100
  -- Reverso.testInv1_1 100
  -- Reverso.testInv1_2 100
  -- Reverso.testInv2_1 100
  -- Reverso.testInv2_2 100
  -- Reverso.testInvLeftSubformulaCmpHeights1_1 100
  -- Reverso.testInvLeftSubformulaCmpHeights1_2 100
  -- Reverso.testInvLeftSubformulaCmpHeights2_1 100
  -- Reverso.testInvLeftSubformulaCmpHeights1_2 100
  -- Reverso.testSubinvoke1_1 100
  -- Reverso.testSubinvoke1_2 100
  -- Reverso.testSubinvoke2_1 100
  -- Reverso.testSubinvoke2_2 100
  -- Reverso.testDefsApprox1_1 100
  -- Reverso.testDefsApprox1_2 100
  -- Reverso.testDefsApprox2_1 100
  -- Reverso.testDefsApprox2_2 100
