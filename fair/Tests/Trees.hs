module Tests.Trees where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding

----------------------------------------------------

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

treeInv :: RunGoal X Invokes
treeInv = RG treeCall

treeEmbed :: RunGoal X Streams
treeEmbed = RG treeCall

treeInvEmbed :: RunGoal X StreamsDict
treeInvEmbed = RG treeCall

treeInvs :: RunGoal X InvokesDict
treeInvs = RG treeCall

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

tests = do
  putStrLn "Conj trees:"
  putStrLn $ show $ run treeVars treeDefs (I 5) treeInv
  putStrLn $ show $ run treeVars treeDefs (sc1 shallowestIgnoringEmbed) treeEmbed
  putStrLn $ show $ run treeVars treeDefs (sc2 shallowestIgnoringEmbed eqAF) treeEmbed
  putStrLn $ show $ run treeVars treeDefs (sc2 shallowIgnoringEmbed eqAF) treeEmbed
  putStrLn $ show $ run treeVars treeDefs (sc2 deepIgnoringEmbed eqAF) treeEmbed
  putStrLn $ show $ run treeVars treeDefs (sc2 shallowestIgnoringSubformula eqAF) treeEmbed
  putStrLn $ show $ run treeVars treeDefs (sc1 shallowestEmbed) treeEmbed
  putStrLn $ show $ run treeVars treeDefs (cmpSD shallowestIgnoringLeftSubformula) treeInvEmbed
  putStrLn $ show $ run treeVars treeDefs () treeInvs
