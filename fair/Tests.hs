module Tests where

import Syntax

import FairStream
import Labels
import Util

import Program.GCW
import Program.Hanoi
import Program.Scheme

---------------------------------------

listAB_def = [Def "list" ["e", "l"] $
  V "l" === C "nil" [] |||
  Fresh "ls" (V "l" === C "cons" [V "e", V "ls"] &&&
              Invoke "list" [V "e", V "ls"])
             ]

listAB_call = Invoke "list" [C "A" [], V "x"] &&& Invoke "list" [C "B" [], V "x"]

listAB_vars = ["x"]

listAB_unit :: RunGoal X ()
listAB_unit = RG listAB_call

listAB_int :: RunGoal X Int
listAB_int = RG listAB_call

listAB_embed :: RunGoal X Streams
listAB_embed = RG listAB_call

---------------------------------------

treeL = Def "treeL" ["t"] $
  V "t" === C "Leaf" [] |||
  Fresh "t'" (V "t" === C "Node" [V "t'", C "Leaf" []] &&&
              Invoke "treeL" [V "t'"])


treeR = Def "treeR" ["t"] $
  V "t" === C "Leaf" [] |||
  Fresh "t'" (V "t" === C "Node" [C "Leaf" [], V "t'"] &&&
              Invoke "treeR" [V "t'"])

treeDefs = [treeL, treeR]

treeCall = Invoke "treeL" [V "t"] &&& Invoke "treeR" [V "t"]

treeVars = ["t"]

treeUnit :: RunGoal X ()
treeUnit = RG treeCall

treeInt :: RunGoal X Int
treeInt = RG treeCall

treeEmbed :: RunGoal X Streams
treeEmbed = RG treeCall

---------------------------------------

int2nat 0 = C "o" []
int2nat n = C "s" [int2nat $ n - 1]

bottlesCall = Fresh "c" $
   Invoke "capacities1" [V "c"] :/\:
   Invoke "checkAnswer" [V "answer", V "c", int2nat 7, C "true" []]

bottlesVars = ["answer"]

bottlesUnit :: RunGoal X ()
bottlesUnit = RG bottlesCall

bottlesInt :: RunGoal X Int
bottlesInt = RG bottlesCall

bottlesDisj :: RunGoal X (Disj, Int)
bottlesDisj = RG bottlesCall

bottlesEmbed :: RunGoal X Streams
bottlesEmbed = RG bottlesCall

---------------------------------------

bridgeCall =
    Invoke "result" [V "minutes"] &&&
    Invoke "getAnswer" [V "answer", C "some" [V "minutes"]]

bridgeVars = ["minutes", "answer"]

bridgeUnit :: RunGoal X ()
bridgeUnit = RG bridgeCall

bridgeInt :: RunGoal X Int
bridgeInt  = RG bridgeCall

bridgeDisj :: RunGoal X (Disj, Int)
bridgeDisj = RG bridgeCall

bridgeSignVars :: RunGoal X (SignVars, Int)
bridgeSignVars = RG bridgeCall

bridgeEmbed :: RunGoal X Streams
bridgeEmbed = RG bridgeCall

---------------------------------------

gcwDefs :: [Def]
gcwDefs = fst $ tree2defs gcw

gcwCall :: G X
gcwCall = Invoke "checkAnswer" [V "answer", C "true" []]

gcwVars = ["answer"]

gcwUnit :: RunGoal X ()
gcwUnit = RG gcwCall

gcwInt :: RunGoal X Int
gcwInt  = RG gcwCall

gcwDisj :: RunGoal X (Disj, Int)
gcwDisj = RG gcwCall

gcwEmbed :: RunGoal X Streams
gcwEmbed = RG gcwCall

---------------------------------------

hanoiDefs :: [Def]
hanoiDefs = fst $ tree2defs hanoi


pins :: Int -> Tx
pins m = C "triple" [gen 0, C "nil" [], C "nil" []] where
  gen n = if n == m then C "nil" [] else C "%" [int2nat n, gen $ n + 1]


hanoiCall :: G X
hanoiCall = Invoke "check" [pins 3, V "answer", C "true" []]

hanoiVars = ["answer"]

hanoiUnit :: RunGoal X ()
hanoiUnit = RG hanoiCall

hanoiInt :: RunGoal X Int
hanoiInt  = RG hanoiCall

hanoiDisj :: RunGoal X (Disj, Int)
hanoiDisj = RG hanoiCall

hanoiEmbed :: RunGoal X Streams
hanoiEmbed = RG hanoiCall

---------------------------------------

schemeDefs :: [Def]
schemeDefs = fst $ tree2defs scheme

schemeCall :: G X
schemeCall = Invoke "eval" [V "quine", C "nil" [], C "val" [V "quine"]]

schemeVars = ["quine"]

schemeUnit :: RunGoal X ()
schemeUnit = RG schemeCall

schemeInt :: RunGoal X Int
schemeInt  = RG schemeCall

schemeDisj :: RunGoal X (Disj, Int)
schemeDisj = RG schemeCall

schemeEmbed :: RunGoal X Streams
schemeEmbed = RG schemeCall