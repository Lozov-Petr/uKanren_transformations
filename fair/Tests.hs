module Tests where

import Syntax
import Fair

listAB_Def = [Def "list" ["e", "l"] $
  V "l" === C "nil" [] |||
  Fresh "ls" (V "l" === C "cons" [V "e", V "ls"] &&&
              Invoke "list" [V "e", V "ls"])
             ]

listAB_Call = Invoke "list" [C "A" [], V 0] &&& Invoke "list" [C "B" [], V 0]

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

tree_call = Invoke "treeL" [V 0] &&& Invoke "treeR" [V 0]

tree_unit :: InitialStream ()
tree_unit = initialState tree_call

tree_int :: InitialStream Int
tree_int = initialState tree_call

---------------------------------------

int2nat 0 = C "o" []
int2nat n = C "s" [int2nat $ n - 1]

bottlesCall =
   Invoke "capacities1" [V 1] :/\:
   Invoke "checkAnswer" [V 0, V 1, int2nat 7, C "true" []]

bottlesUnit :: InitialStream ()
bottlesUnit = initialState bottlesCall

bottlesInt :: InitialStream Int
bottlesInt = initialState bottlesCall

bottlesDisj :: InitialStream Disj
bottlesDisj = initialState bottlesCall

---------------------------------------

bridgeCall =
    Invoke "result" [V 1] &&&
    Invoke "getAnswer" [V 0, C "some" [V 1]]

bridgeUnit :: InitialStream ()
bridgeUnit = initialState bridgeCall

bridgeInt :: InitialStream Int
bridgeInt  = initialState bridgeCall

bridgeDisj :: InitialStream Disj
bridgeDisj = initialState bridgeCall

bridgeVars :: InitialStream SignVars
bridgeVars = initialState bridgeCall
