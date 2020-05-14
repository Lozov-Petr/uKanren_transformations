module Main where

import qualified Tests.ListAB      as ListAB
import qualified Tests.Trees       as Trees
-- import qualified Tests.Bottles     as Bottles
-- import qualified Tests.Bridge      as Bridge
-- import qualified Tests.GCW         as GCW
-- import qualified Tests.Hanoi       as Hanoi
-- import qualified Tests.Scheme      as Scheme
-- import qualified Tests.Sudoku4x4   as Sudoku4x4
-- import qualified Tests.Reverso     as Reverso
-- import qualified Tests.Sorto       as Sorto
-- import qualified Tests.BadExamples as Bad

----------------------------------------------------

main = do
  ListAB.tests
  Trees.tests

  -- Bottles.testInvsSubinvoke_NonStrict
  -- Bottles.testInvsSubinvoke_Strict
  -- Bottles.testDefsApprox
  -- Bottles.testUnfoldSimpl
  -- Bottles.testUnfoldDefsRating
  -- Bottles.testUnfoldFirstGoodCall
  -- Bottles.testUnfoldEssentialArgs
  -- Bottles.testUnfoldingFairConj

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
  -- Bridge.testUnfoldSimpl
  -- Bridge.testUnfoldSimpl'
  -- Bridge.testUnfoldDefsRating
  -- Bridge.testUnfoldDefsRating'
  -- Bridge.testUnfoldFirstGoodCall
  -- Bridge.testUnfoldFirstGoodCall'
  -- Bridge.testUnfoldEssentialArgs
  -- Bridge.testUnfoldEssentialArgs'
  -- Bridge.testUnfoldingFairConj
  -- Bridge.testUnfoldingFairConj'

  -- GCW.testUnit
  -- GCW.testUnfoldSimpl
  -- GCW.testUnfoldDefsRating
  -- GCW.testUnfoldFirstGoodCall

  -- Hanoi.testInvLeftSubformula
  -- Hanoi.testInvLeftSubformulaCmpHeights
  -- Hanoi.testInvsSubinvoke_NonStrict
  -- Hanoi.testInvsSubinvoke_Strict
  -- Hanoi.testDefsApprox
  -- Hanoi.testUnfoldSimpl
  -- Hanoi.testUnfoldDefsRating
  -- Hanoi.testUnfoldFirstGoodCall
  -- Hanoi.testUnfoldEssentialArgs
  -- Hanoi.testUnfoldingFairConj

  -- Scheme.testInvLeftSubformula
  -- Scheme.testInvLeftSubformulaCmpHeights
  -- Scheme.testInvsSubinvoke_NonStrict
  -- Scheme.testInvsSubinvoke_Strict
  -- Scheme.testDefsApprox
  -- Scheme.testUnfoldSimpl
  -- Scheme.testUnfoldDefsRating
  -- Scheme.testUnfoldFirstGoodCall
  -- Scheme.testUnfoldEssentialArgs
  -- Scheme.testUnfoldingFairConj

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
  -- Reverso.testUnfoldSimpl1_1 100
  -- Reverso.testUnfoldSimpl1_2 100
  -- Reverso.testUnfoldSimpl2_1 100
  -- Reverso.testUnfoldSimpl2_2 100
  -- Reverso.testUnfoldDefsRating1_1 100
  -- Reverso.testUnfoldDefsRating1_2 100
  -- Reverso.testUnfoldDefsRating2_1 100
  -- Reverso.testUnfoldDefsRating2_2 100
  -- Reverso.testUnfoldEssentialArgs1_1 100
  -- Reverso.testUnfoldEssentialArgs1_2 100
  -- Reverso.testUnfoldEssentialArgs2_1 100
  -- Reverso.testUnfoldEssentialArgs2_2 100
  -- Reverso.testUnfoldingFairConj1_1 100
  -- Reverso.testUnfoldingFairConj1_2 100
  -- Reverso.testUnfoldingFairConj2_1 100
  -- Reverso.testUnfoldingFairConj2_2 100

  -- Sorto.testUnit1 30
  -- Sorto.testUnit2 30
  -- Sorto.testDefsApprox1 30
  -- Sorto.testDefsApprox2 30
  -- Sorto.testUnfoldSimpl1 4
  -- Sorto.testUnfoldSimpl2 4
  -- Sorto.testUnfoldDefsRating1 4
  -- Sorto.testUnfoldDefsRating2 4
  -- Sorto.testUnfoldFirstGoodCall1 4
  -- Sorto.testUnfoldFirstGoodCall2 4
  -- Sorto.testUnfoldEssentialArgs1 4
  -- Sorto.testUnfoldEssentialArgs2 4
  -- Sorto.testUnfoldingFairConj1 4
  -- Sorto.testUnfoldingFairConj2 4

  -- Bad.testUnfoldSimpl1 5
  -- Bad.testUnfoldSimpl2 5
  -- Bad.testUnfoldDefsRating1 5
  -- Bad.testUnfoldDefsRating2 5
  -- Bad.testUnfoldFirstGoodCall1 5
  -- Bad.testUnfoldFirstGoodCall2 5
  -- Bad.testUnfoldEssentialArgs1 5
  -- Bad.testUnfoldEssentialArgs2 5
  -- Bad.testUnfoldingFairConj1 5
  -- Bad.testUnfoldingFairConj2 5
