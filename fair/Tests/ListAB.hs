module Tests.ListAB where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding

----------------------------------------------------

defs :: [Def]
defs = [Def "list" ["e", "l"] $
  V "l" === C "nil" [] |||
  Fresh "ls" (V "l" === C "cons" [V "e", V "ls"] &&&
              Invoke "list" [V "e", V "ls"])
             ]

goal = Invoke "list" [C "A" [], V "x"] &&& Invoke "list" [C "B" [], V "x"]

vars = ["x"]

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
  putStrLn "Conj lists:"
  putStrLn $ show $ run vars defs (I 5) goalInv
  putStrLn $ show $ run vars defs (sc1 shallowestIgnoringEmbed) goalEmbed
  putStrLn $ show $ run vars defs (sc2 shallowestIgnoringEmbed eqAF) goalEmbed
  putStrLn $ show $ run vars defs (sc2 shallowIgnoringEmbed eqAF) goalEmbed
  putStrLn $ show $ run vars defs (sc2 deepIgnoringEmbed eqAF) goalEmbed
  putStrLn $ show $ run vars defs (sc2 shallowestIgnoringSubformula eqAF) goalEmbed
  putStrLn $ show $ run vars defs (sc1 shallowestEmbed) goalEmbed
  putStrLn $ show $ run vars defs (cmpSD shallowestIgnoringLeftSubformula) goalInvEmbed
  putStrLn $ show $ run vars defs () goalInvs