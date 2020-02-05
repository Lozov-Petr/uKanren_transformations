module Tests where

import Syntax
import Fair

listAB_Def = [Def "list" ["e", "l"] $
  V "l" === C "nil" [] ||| 
  Fresh "ls" (V "l" === C "cons" [V "e", V "ls"] &&& 
              Invoke "list" [V "e", V "ls"])
             ]
               
listAB_Call = Fresh "x" $ Invoke "list" [C "A" [], V "x"] &&& Invoke "list" [C "B" [], V "x"]

listAB_Unit :: Stream ()
listAB_Unit = initialState listAB_Call

listAB_Int :: Stream Int
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

tree_unit :: Stream ()
tree_unit = initialState tree_call

tree_int :: Stream Int
tree_int = initialState tree_call

---------------------------------------

int2nat 0 = C "o" []
int2nat n = C "s" [int2nat $ n - 1]

bottlesCall = 
  fresh ["x", "y"] $
   Invoke "capacities1" [V "y"] :/\: 
   Invoke "checkAnswer" [V "x", V "y", int2nat 7, C "true" []]
         
bottlesUnit :: Stream ()
bottlesUnit = initialState bottlesCall

bottlesInt :: Stream Int
bottlesInt = initialState bottlesCall

---------------------------------------

bridgeCall =
  fresh ["x", "answer", "res"] $
    V "x" === C "Answer" [V "answer", V "res"] &&&
    Invoke "result" [V "res"] &&&
    Invoke "getAnswer" [V "answer", C "some" [V "res"]]
    
bridgeUnit :: Stream ()
bridgeUnit = initialState bridgeCall

bridgeInt :: Stream Int
bridgeInt = initialState bridgeCall