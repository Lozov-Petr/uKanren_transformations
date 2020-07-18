module Program.BridgeAutogen where

import Syntax

def = Def

tree =
 (\last_goal ->
  Let (def "eqBool" ["x", "y", "q133"] (
    ((V "x" === C "true" []) &&&
    (V "y" === V "q133")) |||
    ((V "x" === C "false" []) &&&
    (((V "y" === C "true" []) &&&
    (V "q133" === C "false" [])) |||
    ((V "y" === C "false" []) &&&
    (V "q133" === C "true" []))))
  )) (
  Let (def "eqState" ["x", "y", "q107"] (
    fresh ["a1", "a2", "a3", "a4", "a5", "b1", "b2", "b3", "b4", "b5", "q109", "q110", "q115", "q116", "q121", "q122", "q127", "q128"] (
      (V "x" === C "st" [V "a1", V "a2", V "a3", V "a4", V "a5"]) &&&
      (V "y" === C "st" [V "b1", V "b2", V "b3", V "b4", V "b5"]) &&&
      (call "eqBool" [V "a1", V "b2", V "q109"]) &&&
      (call "eqBool" [V "a2", V "b2", V "q115"]) &&&
      (call "eqBool" [V "a3", V "b3", V "q121"]) &&&
      (call "eqBool" [V "a4", V "b4", V "q127"]) &&&
      (call "eqBool" [V "a5", V "b5", V "q128"]) &&&
      (((V "q127" === C "false" []) &&&
      (V "q122" === C "false" [])) |||
      ((V "q127" === C "true" []) &&&
      (V "q122" === V "q128"))) &&&
      (((V "q121" === C "false" []) &&&
      (V "q116" === C "false" [])) |||
      ((V "q121" === C "true" []) &&&
      (V "q116" === V "q122"))) &&&
      (((V "q115" === C "false" []) &&&
      (V "q110" === C "false" [])) |||
      ((V "q115" === C "true" []) &&&
      (V "q110" === V "q116"))) &&&
      (((V "q109" === C "false" []) &&&
      (V "q107" === C "false" [])) |||
      ((V "q109" === C "true" []) &&&
      (V "q107" === V "q110"))))
  )) (
  Let (def "greater" ["a0", "b0", "q103"] (
    ((V "a0" === C "o" []) &&&
    (V "q103" === C "false" [])) |||
    (fresh ["x"] (
       (V "a0" === C "s" [V "x"]) &&&
       (((V "b0" === C "o" []) &&&
       (V "q103" === C "true" [])) |||
       (fresh ["y"] (
          (V "b0" === C "s" [V "y"]) &&&
          (call "greater" [V "x", V "y", V "q103"]))))))
  )) (
  Let (def "grForPerson" ["x", "y", "q86"] (
    ((V "x" === C "a" []) &&&
    (((V "y" === C "a" []) &&&
    (V "q86" === C "false" [])) |||
    ((V "y" === C "b" []) &&&
    (V "q86" === C "true" [])) |||
    ((V "y" === C "c" []) &&&
    (V "q86" === C "true" [])) |||
    ((V "y" === C "d" []) &&&
    (V "q86" === C "true" [])))) |||
    ((V "x" === C "b" []) &&&
    (((V "y" === C "a" []) &&&
    (V "q86" === C "false" [])) |||
    ((V "y" === C "b" []) &&&
    (V "q86" === C "false" [])) |||
    ((V "y" === C "c" []) &&&
    (V "q86" === C "false" [])) |||
    ((V "y" === C "d" []) &&&
    (V "q86" === C "true" [])))) |||
    ((V "x" === C "c" []) &&&
    (((V "y" === C "a" []) &&&
    (V "q86" === C "false" [])) |||
    ((V "y" === C "b" []) &&&
    (V "q86" === C "false" [])) |||
    ((V "y" === C "c" []) &&&
    (V "q86" === C "false" [])) |||
    ((V "y" === C "d" []) &&&
    (V "q86" === C "true" [])))) |||
    ((V "x" === C "d" []) &&&
    (V "q86" === C "false" []))
  )) (
  Let (def "max" ["a0", "b0", "q82"] (
    fresh ["q83"] (
      (call "greater" [V "a0", V "b0", V "q83"]) &&&
      (((V "q83" === C "true" []) &&&
      (V "a0" === V "q82")) |||
      ((V "q83" === C "false" []) &&&
      (V "b0" === V "q82"))))
  )) (
  Let (def "add" ["a0", "b0", "q80"] (
    ((V "a0" === C "o" []) &&&
    (V "b0" === V "q80")) |||
    (fresh ["x"] (
       (V "a0" === C "s" [V "x"]) &&&
       (call "add" [V "x", C "s" [V "b0"], V "q80"])))
  )) (
  Let (def "checkPerson" ["state", "person", "q78"] (
    fresh ["l", "a0", "b0", "c0", "d0"] (
      (V "state" === C "st" [V "l", V "a0", V "b0", V "c0", V "d0"]) &&&
      (((V "person" === C "a" []) &&&
      (call "eqBool" [V "a0", V "l", V "q78"])) |||
      ((V "person" === C "b" []) &&&
      (call "eqBool" [V "b0", V "l", V "q78"])) |||
      ((V "person" === C "c" []) &&&
      (call "eqBool" [V "c0", V "l", V "q78"])) |||
      ((V "person" === C "d" []) &&&
      (call "eqBool" [V "d0", V "l", V "q78"]))))
  )) (
  Let (def "checkStep" ["state", "step", "q65"] (
    (fresh ["p"] (
       (V "step" === C "one" [V "p"]) &&&
       (call "checkPerson" [V "state", V "p", V "q65"]))) |||
    (fresh ["p", "q", "q66", "q67", "q72", "q73"] (
       (V "step" === C "two" [V "p", V "q"]) &&&
       (call "checkPerson" [V "state", V "p", V "q66"]) &&&
       (call "checkPerson" [V "state", V "q", V "q72"]) &&&
       (call "grForPerson" [V "p", V "q", V "q73"]) &&&
       (((V "q72" === C "false" []) &&&
       (V "q67" === C "false" [])) |||
       ((V "q72" === C "true" []) &&&
       (V "q67" === V "q73"))) &&&
       (((V "q66" === C "false" []) &&&
       (V "q65" === C "false" [])) |||
       ((V "q66" === C "true" []) &&&
       (V "q65" === V "q67")))))
  )) (
  Let (def "moveLight" ["state", "q60"] (
    fresh ["l", "a0", "b0", "c0", "d0", "q61"] (
      (V "state" === C "st" [V "l", V "a0", V "b0", V "c0", V "d0"]) &&&
      (V "q60" === C "st" [V "q61", V "a0", V "b0", V "c0", V "d0"]) &&&
      (((V "l" === C "true" []) &&&
      (V "q61" === C "false" [])) |||
      ((V "l" === C "false" []) &&&
      (V "q61" === C "true" []))))
  )) (
  Let (def "movePerson" ["state", "person", "q42"] (
    fresh ["l", "a0", "b0", "c0", "d0"] (
      (V "state" === C "st" [V "l", V "a0", V "b0", V "c0", V "d0"]) &&&
      ((fresh ["q44"] (
          (V "person" === C "a" []) &&&
          (V "q42" === C "st" [V "l", V "q44", V "b0", V "c0", V "d0"]) &&&
          (((V "a0" === C "true" []) &&&
          (V "q44" === C "false" [])) |||
          ((V "a0" === C "false" []) &&&
          (V "q44" === C "true" []))))) |||
      (fresh ["q48"] (
         (V "person" === C "b" []) &&&
         (V "q42" === C "st" [V "l", V "a0", V "q48", V "c0", V "d0"]) &&&
         (((V "b0" === C "true" []) &&&
         (V "q48" === C "false" [])) |||
         ((V "b0" === C "false" []) &&&
         (V "q48" === C "true" []))))) |||
      (fresh ["q52"] (
         (V "person" === C "c" []) &&&
         (V "q42" === C "st" [V "l", V "a0", V "b0", V "q52", V "d0"]) &&&
         (((V "c0" === C "true" []) &&&
         (V "q52" === C "false" [])) |||
         ((V "c0" === C "false" []) &&&
         (V "q52" === C "true" []))))) |||
      (fresh ["q56"] (
         (V "person" === C "d" []) &&&
         (V "q42" === C "st" [V "l", V "a0", V "b0", V "c0", V "q56"]) &&&
         (((V "d0" === C "true" []) &&&
         (V "q56" === C "false" [])) |||
         ((V "d0" === C "false" []) &&&
         (V "q56" === C "true" [])))))))
  )) (
  Let (def "step" ["state", "step", "q35"] (
    (fresh ["p", "q36"] (
       (V "step" === C "one" [V "p"]) &&&
       (call "movePerson" [V "state", V "p", V "q36"]) &&&
       (call "moveLight" [V "q36", V "q35"]))) |||
    (fresh ["p", "q", "q38", "q40"] (
       (V "step" === C "two" [V "p", V "q"]) &&&
       (call "movePerson" [V "state", V "p", V "q40"]) &&&
       (call "movePerson" [V "q40", V "q", V "q38"]) &&&
       (call "moveLight" [V "q38", V "q35"])))
  )) (
  Let (def "times" ["p", "q30"] (
    ((V "p" === C "a" []) &&&
    (V "q30" === C "s" [C "o" []])) |||
    ((V "p" === C "b" []) &&&
    (V "q30" === C "s" [C "s" [C "o" []]])) |||
    ((V "p" === C "c" []) &&&
    (V "q30" === C "s" [C "s" [C "s" [C "s" [C "s" [C "o" []]]]]])) |||
    ((V "p" === C "d" []) &&&
    (V "q30" === C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "o" []]]]]]]]]]]))
  )) (
  Let (def "getTime" ["state", "q26"] (
    (fresh ["p"] (
       (V "state" === C "one" [V "p"]) &&&
       (call "times" [V "p", V "q26"]))) |||
    (fresh ["p", "q", "q27", "q28"] (
       (V "state" === C "two" [V "p", V "q"]) &&&
       (call "times" [V "p", V "q27"]) &&&
       (call "times" [V "q", V "q28"]) &&&
       (call "max" [V "q27", V "q28", V "q26"])))
  )) (
  Let (def "getAnswer" ["answer", "q25"] (
    Let (def "start" ["q0"] (
      V "q0" === C "st" [C "true" [], C "true" [], C "true" [], C "true" [], C "true" []]
    )) (
    Let (def "finish" ["q1"] (
      V "q1" === C "st" [C "false" [], C "false" [], C "false" [], C "false" [], C "false" []]
    )) (
    Let (def "getAnswer'" ["answer", "state", "q2"] (
      (fresh ["x", "xs", "q4"] (
         (V "answer" === C "%" [V "x", V "xs"]) &&&
         (call "checkStep" [V "state", V "x", V "q4"]) &&&
         ((fresh ["q6", "q12"] (
             (V "q4" === C "true" []) &&&
             (call "step" [V "state", V "x", V "q12"]) &&&
             (call "getAnswer'" [V "xs", V "q12", V "q6"]) &&&
             (((V "q6" === C "none" []) &&&
             (V "q2" === C "none" [])) |||
             (fresh ["t1", "q8", "q10"] (
                (V "q6" === C "some" [V "t1"]) &&&
                (V "q2" === C "some" [V "q8"]) &&&
                (call "getTime" [V "x", V "q10"]) &&&
                (call "add" [V "q10", V "t1", V "q8"])))))) |||
         ((V "q4" === C "false" []) &&&
         (V "q2" === C "none" []))))) |||
      (fresh ["q16", "q19"] (
         (V "answer" === C "nil" []) &&&
         (call "finish" [V "q19"]) &&&
         (call "eqState" [V "state", V "q19", V "q16"]) &&&
         (((V "q16" === C "true" []) &&&
         (V "q2" === C "some" [C "o" []])) |||
         ((V "q16" === C "false" []) &&&
         (V "q2" === C "none" [])))))
    )) (
    fresh ["q21"] (
      (call "start" [V "q21"]) &&&
      (call "getAnswer'" [V "answer", V "q21", V "q25"])))))
  )) (
  last_goal))))))))))))))

  ,

  "open GT\nopen OCanren\nopen OCanren.Std\ntype 'a0 gpeano =\n  | O \n  | S of 'a0 \nmodule For_gpeano =\n  (Fmap)(struct\n           let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\n           type 'a0 t = 'a0 gpeano\n         end)\nlet rec o () = inj (For_gpeano.distrib O)\nand s x__0 = inj (For_gpeano.distrib (S x__0))\ntype person =\n  | A \n  | B \n  | C \n  | D \nlet a () = !! A\nlet b () = !! B\nlet c () = !! C\nlet d () = !! D\ntype 'a0 gstep =\n  | One of 'a0 \n  | Two of 'a0 * 'a0 \nmodule For_gstep =\n  (Fmap)(struct\n           let rec fmap fa0 =\n             function\n             | One a0 -> One (fa0 a0)\n             | Two (a0_0, a0_1) -> Two ((fa0 a0_0), (fa0 a0_1))\n           type 'a0 t = 'a0 gstep\n         end)\nlet rec one x__0 = inj (For_gstep.distrib (One x__0))\nand two x__0 x__1 = inj (For_gstep.distrib (Two (x__0, x__1)))\ntype 'a0 gstate =\n  | St of 'a0 * 'a0 * 'a0 * 'a0 * 'a0 \nmodule For_gstate =\n  (Fmap)(struct\n           let rec fmap fa0 =\n             function\n             | St (a0_0, a0_1, a0_2, a0_3, a0_4) ->\n                 St\n                   ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2), (fa0 a0_3),\n                     (fa0 a0_4))\n           type 'a0 t = 'a0 gstate\n         end)\nlet rec st x__0 x__1 x__2 x__3 x__4 =\n  inj (For_gstate.distrib (St (x__0, x__1, x__2, x__3, x__4)))")
