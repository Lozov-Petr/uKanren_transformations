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
-- import qualified Tests.Sorto     as Sorto

----------------------------------------------------

main = do
  ListAB.tests
  Trees.tests

  -- Bottles.testInvsSubinvoke_NonStrict
  -- Bottles.testInvsSubinvoke_Strict
  -- Bottles.testDefsApprox

  -- Bridge.testUnit
  -- Bridge.testUnit'
  -- Bridge.testDisj10
  -- Bridge.testInvLeftSubformula
  -- Bridge.testInvLeftSubformulaCmpHeights
  -- Bridge.testInvsSubinvoke_NonStrict
  -- Bridge.testInvsSubinvoke'_NonStrict
  -- Bridge.testInvsSubinvoke_Strict
  -- Bridge.testInvsSubinvoke'_Strict
  -- Bridge.testDefsApprox
  -- Bridge.testDefsApprox'

  -- Hanoi.testInvLeftSubformula
  -- Hanoi.testInvLeftSubformulaCmpHeights
  -- Hanoi.testInvsSubinvoke_NonStrict
  -- Hanoi.testInvsSubinvoke_Strict
  -- Hanoi.testDefsApprox

  -- Scheme.testInvLeftSubformula
  -- Scheme.testInvLeftSubformulaCmpHeights
  -- Scheme.testInvsSubinvoke_NonStrict
  -- Scheme.testInvsSubinvoke_Strict
  -- Scheme.testDefsApprox

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
  -- Reverso.testSubinvoke1_1_NonStritct 100
  -- Reverso.testSubinvoke1_2_NonStritct 100
  -- Reverso.testSubinvoke2_1_NonStritct 100
  -- Reverso.testSubinvoke2_2_NonStritct 100
  -- Reverso.testSubinvoke1_1_Stritct 100
  -- Reverso.testSubinvoke1_2_Stritct 100
  -- Reverso.testSubinvoke2_1_Stritct 100
  -- Reverso.testSubinvoke2_2_Stritct 100
  -- Reverso.testDefsApprox1_1 100
  -- Reverso.testDefsApprox1_2 100
  -- Reverso.testDefsApprox2_1 100
  -- Reverso.testDefsApprox2_2 100

  -- Sorto.testUnit1 30
  -- Sorto.testUnit2 30
  -- Sorto.testDefsApprox1 30
  -- Sorto.testDefsApprox2 30
