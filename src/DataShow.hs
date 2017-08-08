module DataShow where
import Data
import Data.List (intercalate)

showSubst f s = "[" ++ intercalate ", " (map (\(i,t) -> f i ++ " -> " ++ show t) s) ++  "]"

instance Show State where
  show (State subst state index vars) = "St " ++ showSubst (\i -> "x." ++ show i) subst ++
--                                        "\n" ++ showSubst id (map (\x -> (x, state x)) vars) ++
                                        " " ++ show index

instance Show Term where
  show (Var v) = "v." ++ v
  show (Free i) = "x." ++ show i
  show (Ctor ctor ts) =
    case ctor of
      "Nil" -> "[]"
      "Cons" -> let [h,t] = ts
                in  "(" ++ show h ++ ":" ++ show t ++ ")"
      x | x `elem` ["Pair", "Triple", "Tuple4", "Tuple5", "Tuple6"] ->
          "(" ++ intercalate ", " (map show ts) ++ ")"
      _ | null ts -> ctor
      _ ->  ctor ++ showList ts ""
  showList ts _ = intercalate "\n" (map show ts)

instance Show Goal where
  show t =
    show' t []
    where
      show' (Fresh v g) fresh = show' g (v:fresh)
      show' g fresh =
        (if null fresh then "" else "Fresh " ++ unwords (reverse fresh) ++ " ") ++
        case g of
          Unify l r -> show l ++ " === " ++ show r
--          Conj  l r -> "(" ++ show l ++ ") &&& (" ++ show r ++ ")"
          Conj  xs -> intercalate "&&&" (map (\x -> "(" ++ show x ++ ")") xs)
          Disj  xs -> intercalate " ||| " (map (\x -> "(" ++ show x ++ ")") xs) --"(" ++ show l ++ ") ||| (" ++ show r ++ ")"
          Invoke n args -> n ++ " " ++ unwords (map show args)
          Zzz g -> "Zzz " ++ show g

instance Show a => Show (Stream a) where
  show Empty = "[]"
  show (Mature a s) = show a ++ " :\n" ++ show s
  show (Immature s) = "Immature " ++ show s

instance Show Tree where
  show t =
    show' t 0
    where
      nSpaces n = replicate n ' '
      show' t n =
        case t of
          Fail                        -> nSpaces n ++ "F"
          Success  st                 -> nSpaces n ++ "S " ++ show st
          Renaming i st g             -> nSpaces n ++ "R " ++ show i ++ " " ++ show st ++ " (" ++ show g ++ ")"
          Step     i st g fv ch       -> nSpaces n ++ "T " ++ show fv ++ " " ++ show i ++ " " ++ show st ++ " (" ++ show g ++ ")" ++ "\n" ++ show' ch (n+1)
          Or       i st g ch          -> nSpaces n ++ "O " ++ show i ++ " " ++ show st ++ " (" ++ show g ++ ")" ++ "\n" ++ intercalate "\n" (map (\x -> show' x (n+1)) ch)
          Split    i st g1 g2 ch1 ch2 -> nSpaces n ++ "G " ++ show i ++ " " ++ show st ++ " (" ++ show g1 ++ ")" ++ " (" ++ show g2 ++ ")" ++ "\n" ++ show' ch1 (n+1) ++ "\n" ++ show' ch2 (n+1)
          Gen      i st subst g ch    -> nSpaces n ++ "A " ++ show i ++ " " ++ show st ++ " <" ++ show subst ++ "> "  ++ " (" ++ show g ++ ")" ++ "\n" ++ show' ch (n+1)

instance Show Def where
  show x = "let " ++ name x ++ " " ++ unwords (args x) ++ " = " ++ show (body x)

instance Show Spec where
  show x = "Spec\nGoal: " ++ show (goal x) ++ "\nDefs: " ++ intercalate "\n" (map show $ defs x)

