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

defs = [treeL, treeR]

goal = Invoke "treeL" [V "t"] &&& Invoke "treeR" [V "t"]

vars = ["t"]

goalUnit :: RunGoal X ()
goalUnit = RG goal

goalInt :: RunGoal X Int
goalInt = RG goal

goalInv :: RunGoal X Invokes
goalInv = RG goal

goalEmbed :: RunGoal X Streams
goalEmbed = RG goal

goalInvEmbed :: RunGoal X StreamsDict
goalInvEmbed = RG goal

goalInvs :: RunGoal X InvokesDict
goalInvs = RG goal

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

tests = do
  putStrLn "Conj trees:"
  putStrLn $ show $ run vars defs (I 5) goalInv
  putStrLn $ show $ run vars defs (sc1 shallowestIgnoringEmbed) goalEmbed
  putStrLn $ show $ run vars defs (sc2 shallowestIgnoringEmbed eqAF) goalEmbed
  putStrLn $ show $ run vars defs (sc2 shallowIgnoringEmbed eqAF) goalEmbed
  putStrLn $ show $ run vars defs (sc2 deepIgnoringEmbed eqAF) goalEmbed
  putStrLn $ show $ run vars defs (sc2 shallowestIgnoringSubformula eqAF) goalEmbed
  putStrLn $ show $ run vars defs (sc1 shallowestEmbed) goalEmbed
  putStrLn $ show $ run vars defs (cmpSD shallowestIgnoringLeftSubformula) goalInvEmbed
  putStrLn $ show $ run vars defs () goalInvs
