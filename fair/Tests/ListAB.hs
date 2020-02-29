module Tests.ListAB where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding

----------------------------------------------------

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

listAB_inv :: RunGoal X Invokes
listAB_inv = RG listAB_call

listAB_embed :: RunGoal X Streams
listAB_embed = RG listAB_call

listAB_invEmbed :: RunGoal X StreamsDict
listAB_invEmbed = RG listAB_call

listAB_invs :: RunGoal X InvokesDict
listAB_invs = RG listAB_call

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

tests = do
  putStrLn "Conj lists:"
  putStrLn $ show $ run listAB_vars listAB_def (I 5) listAB_inv
  putStrLn $ show $ run listAB_vars listAB_def (sc1 shallowestIgnoringEmbed) listAB_embed
  putStrLn $ show $ run listAB_vars listAB_def (sc2 shallowestIgnoringEmbed eqAF) listAB_embed
  putStrLn $ show $ run listAB_vars listAB_def (sc2 shallowIgnoringEmbed eqAF) listAB_embed
  putStrLn $ show $ run listAB_vars listAB_def (sc2 deepIgnoringEmbed eqAF) listAB_embed
  putStrLn $ show $ run listAB_vars listAB_def (sc2 shallowestIgnoringSubformula eqAF) listAB_embed
  putStrLn $ show $ run listAB_vars listAB_def (sc1 shallowestEmbed) listAB_embed
  putStrLn $ show $ run listAB_vars listAB_def (cmpSD shallowestIgnoringLeftSubformula) listAB_invEmbed
  putStrLn $ show $ run listAB_vars listAB_def () listAB_invs
