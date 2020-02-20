module Tests.Reverso where

import Syntax

import FairEval
import FairStream
import Labels

import Program.List

----------------------------------------------------

def2 :: Def
def2 = Def "reverso" ["x", "y"] $
  (V "x" === nil &&& V "y" === nil) |||
  fresh ["e", "xs", "ys"] (
    V "x" === V "e" % V "xs" &&&
    call "appendo" [V "ys", V "e" % nil, V "y"] &&&
    call "reverso" [V "xs", V "ys"])

defs1 :: [Def]
defs1 = reverso

defs2 :: [Def]
defs2 = def2 : appendo

genList :: Int -> Tx
genList 0 = nil
genList n = C (show $ n - 1) [] % genList (n - 1)

call1 :: Int -> G X
call1 n = call "reverso" [genList n, V "answer"]

call2 :: Int -> G X
call2 n = call "reverso" [V "answer", genList n]

vars = ["answer"]

call1Unit :: Int -> RunGoal X ()
call1Unit = RG . call1

call1Int :: Int -> RunGoal X Int
call1Int  = RG . call1

call1Disj :: Int -> RunGoal X (Disj, Int)
call1Disj = RG . call1

call1Embed :: Int -> RunGoal X Streams
call1Embed = RG . call1

call1InvEmbed :: Int -> RunGoal X StreamsDict
call1InvEmbed = RG . call1

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

testUnit1 =
  putStrLn . show . takeAnswers 1 . run vars defs1 () . call1Unit
