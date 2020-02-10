module Tests where

import Syntax
import Fair

listAB_Def = [Def "list" ["e", "l"] $
  V "l" === C "nil" [] |||
  Fresh "ls" (V "l" === C "cons" [V "e", V "ls"] &&&
              Invoke "list" [V "e", V "ls"])
             ]

listAB_Call = Fresh "x" $ Invoke "list" [C "A" [], V "x"] &&& Invoke "list" [C "B" [], V "x"]

listAB_Unit :: InitialStream ()
listAB_Unit = initialState listAB_Call

listAB_Int :: InitialStream Int
listAB_Int = initialState listAB_Call

---------------------------------------

treeL = Def "treeL" ["t"] $
  V "t" === C "Leaf" [] |||
  Fresh "t'" (V "t" === C "Node" [V "t'", C "Leaf" []] &&&
              Invoke "treeL" [V "t'"])


treeR = Def "treeR" ["t"] $
  V "t" === C "Leaf" [] |||
  Fresh "t'" (V "t" === C "Node" [C "Leaf" [], V "t'"] &&&
              Invoke "treeR" [V "t'"])

tree_defs = [treeL, treeR]

tree_call = Fresh "x" $ Invoke "treeL" [V "x"] &&& Invoke "treeR" [V "x"]

tree_unit :: InitialStream ()
tree_unit = initialState tree_call

tree_int :: InitialStream Int
tree_int = initialState tree_call

---------------------------------------

int2nat 0 = C "o" []
int2nat n = C "s" [int2nat $ n - 1]

bottlesCall =
  fresh ["c"] $
   Invoke "capacities1" [V "c"] :/\:
   Invoke "checkAnswer" [V "answer", V "c", int2nat 7, C "true" []]

bottlesUnit :: InitialStream ()
bottlesUnit = initialState bottlesCall

bottlesInt :: InitialStream Int
bottlesInt = initialState bottlesCall

bottlesDisj :: InitialStream Disj
bottlesDisj = initialState bottlesCall

---------------------------------------

bridgeCall =
    Invoke "result" [V "res"] &&&
    Invoke "getAnswer" [V "answer", C "some" [V "res"]]

bridgeUnit :: InitialStream ()
bridgeUnit = initialState bridgeCall

bridgeInt :: InitialStream Int
bridgeInt  = initialState bridgeCall

bridgeDisj :: InitialStream Disj
bridgeDisj = initialState bridgeCall

bridgeVars :: InitialStream SignVars
bridgeVars = initialState bridgeCall
