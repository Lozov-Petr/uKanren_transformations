module Main where

import qualified Tests.ListAB    as ListAB
import qualified Tests.Trees     as Trees
-- import qualified Tests.Bottles   as Bottles
-- import qualified Tests.Bridge    as Bridge
-- import qualified Tests.GCW       as GCW
-- import qualified Tests.Hanoi     as Hanoi
-- import qualified Tests.Scheme    as Scheme
-- import qualified Tests.Sudoku4x4 as Sudoku4x4

----------------------------------------------------

main = do
  ListAB.tests
  Trees.tests