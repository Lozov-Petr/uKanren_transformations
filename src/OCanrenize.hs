{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module OCanrenize where

import System.Process
import System.IO
import System.IO.Temp
import Data.Char
import Data.List (intercalate)
import Num
import Sort
import Syntax
import Driving
import Residualize

class OCanren a where
  ocanren :: a -> String

instance OCanren String where
  ocanren = id

instance OCanren v => OCanren (Term v) where
  ocanren (V v)        = ocanren v
  ocanren (C "Nil" _) = "nil ()"
  ocanren (C "Cons" [h,t]) = ocanren h ++ " % " ++ ocanren t
  ocanren (C "O" []) = "zero"
  ocanren (C "S" [x]) = "succ (" ++ ocanren x ++ ")"
  ocanren (C (f:o) ts) = "(" ++ (toLower f : o) ++ case ts of 
                                                     [] -> " ()" 
                                                     _  -> concat [' ' : ocanren t | t <- ts]
                         ++ ")"

instance OCanren v => OCanren (G v) where
  ocanren (t1 :=:  t2)  = "(" ++ ocanren t1 ++ " === " ++ ocanren t2 ++ ")"
  ocanren (g1 :/\: g2)  = "(" ++ ocanren g1 ++ " &&& " ++ ocanren g2 ++ ")"
  ocanren (g1 :\/: g2)  = "(" ++ ocanren g1 ++ " ||| " ++ ocanren g2 ++ ")"
  ocanren (Fresh x g )  = let (names, goal) = freshVars [x] g in "(" ++ "fresh (" ++ intercalate " " names ++ ") (" ++ ocanren goal ++ "))"
  ocanren (Invoke f ts) = "(" ++ f ++ concat [' ' : ocanren t | t <- ts] ++ ")"
  ocanren (Let (n, as, b) g) = "let rec " ++ n ++ concat [' ' : a | a <- as] ++ " = " ++ ocanren b ++ " in defer(" ++ ocanren g ++ ")"


ocanrenize :: String -> [String] -> G X -> String
ocanrenize topLevelName args g = 
  "let " ++ topLevelName ++ " " ++ intercalate " " args ++ " = " ++ ocanren g

toOCanren filename topLevelName (tree, args) =
  do
    withSystemTempFile filename (\ tmp_name tmp -> 
                                   do
                                     hPutStrLn tmp (ocanrenize topLevelName args tree)
                                     hClose tmp
                                     file <- openFile filename WriteMode
                                     hPutStrLn file "open GT"
                                     hPutStrLn file "open MiniKanren"
                                     hPutStrLn file "open Std"
                                     hPutStrLn file "open Nat"
                                     hPutStrLn file "" 
                                     hClose file
                                     system $ "camlp5o pr_o.cmo " ++ tmp_name ++ " >> " ++ filename
                                     system $ "ocamlformat " ++ filename ++ " -m 160 -i"
                                     return ()
                                )

test = toOCanren "appendo2.ml" "appendo2" $ residualize tc 

test' = toOCanren "reverso.ml" "reverso" $ residualize tc'

test'' = toOCanren "revacco.ml" "revacco" $ residualize tc''

test_gto = toOCanren "gto.ml" "gto" $ residualize $ drive $ gto $ fresh ["q", "p", "r"] (call "gto" [V "p", V "r", V "q"])

test_leo = toOCanren "leo.ml" "leo" $ residualize $ drive $ leo $ fresh ["q", "p", "r"] (call "leo" [V "p", V "r", V "q"])

test_smallesto = toOCanren "smallesto.ml" "smallesto" $ residualize $ drive $ smallesto $ fresh ["q", "p", "r"] (call "smallesto" [V "q", V "p", V "r"])

test_sorto = toOCanren "sorto.ml" "sorto" $ residualize $ drive $ sorto $ fresh ["q", "r"] (call "sorto" [V "q", V "r"])

test_minmax = toOCanren "minmaxo.ml" "minmaxo" $ residualize $ drive $ minmaxo $ fresh ["q", "p", "r", "s"] (call "minmaxo" [V "q", V "p", V "r", V "s"])


main = do 
  test 
  test'
  test''
  test_gto
  test_leo
--  test_smallesto
  test_sorto
  test_minmax
