module Tests where

import Syntax

import FairStream
import Labels

listAB_Def = [Def "list" ["e", "l"] $
  V "l" === C "nil" [] |||
  Fresh "ls" (V "l" === C "cons" [V "e", V "ls"] &&&
              Invoke "list" [V "e", V "ls"])
             ]

listAB_Call = Invoke "list" [C "A" [], V "x"] &&& Invoke "list" [C "B" [], V "x"]

listAB_Vars = ["x"]

listAB_Unit :: RunGoal X ()
listAB_Unit = RG listAB_Call

listAB_Int :: RunGoal X Int
listAB_Int = RG listAB_Call

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

tree_int :: RunGoal X Int
tree_int = RG treeCall

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

bottlesDisj :: RunGoal X Disj
bottlesDisj = RG bottlesCall

---------------------------------------

bridgeCall =
    Invoke "result" [V "minutes"] &&&
    Invoke "getAnswer" [V "answer", C "some" [V "minutes"]]

bridgeVars = ["minutes", "answer"]

bridgeUnit :: RunGoal X ()
bridgeUnit = RG bridgeCall

bridgeInt :: RunGoal X Int
bridgeInt  = RG bridgeCall

bridgeDisj :: RunGoal X Disj
bridgeDisj = RG bridgeCall

bridgeSignVars :: RunGoal X SignVars
bridgeSignVars = RG bridgeCall
