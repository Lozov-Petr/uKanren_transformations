module Programs where

import List
import Num
import Syntax

palindromo g = let x = V "x" in Let (def "palindromo" ["x"] (call "reverso" [x, x])) $ reverso g

doubleAppendo g =
  let x = V "x" in
  let y = V "y" in
  let z = V "z" in
  let r = V "r" in
  let t = V "t" in
  Let (def "doubleAppendo" ["x", "y", "z"]
        (
          fresh ["t"] ( call "appendo" [x, y, t] &&& call "appendo" [t, z, r] )
        )
      ) $ appendo g

eveno g = 
  let x = V "x" in 
  let z = V "z" in 
  Let (def "eveno" ["x"] (fresh ["z"] (call "addo" [z, z, x]))) $ addo g

doubleo g = 
  let x = V "x" in 
  let xx = V "xx" in 
  Let (def "doubleo" ["x", "xx"] (call "appendo" [x, x, xx])) $ appendo g

emptyAppendo g = 
  let x = V "x" in 
  let y = V "y" in 
  Let (def "emptyAppendo" ["x", "y"] (call "appendo" [nil, x, y])) $ appendo g

singletonReverso g = 
  let x = V "x" in 
  let y = V "y" in
  Let (def "singletonReverso" ["x", "y"] (fresh ["l"] (call "lengtho" [x, peanify 1] &&& call "reverso" [x, y]))) $ reverso $ lengtho g
