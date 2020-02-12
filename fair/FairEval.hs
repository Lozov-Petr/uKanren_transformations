module FairEval where

import Text.Printf
import Debug.Trace

import Syntax
import qualified Eval

import FairStream
import Util

---------------------------------------

toSemTs :: [(X, Ts)] -> [Tx] -> Er [Ts]
toSemTs e l = foldr combine (return []) l where
  combine x l = toSemT e x >>= \x -> l >>= \l -> return $ x : l

toSemT :: [(X, Ts)] -> Tx -> Er Ts
toSemT e (V x) = case lookup x e of
    Nothing -> Left $ printf "Unexpected variable '%s'." x
    Just x  -> return x
toSemT e (C n a) = toSemTs e a >>= return . (C n)

toSemG :: [(X, Ts)] -> Int -> G X -> Er (G S, Int)
toSemG e i (t1 :=: t2)  = do t1' <- toSemT e t1
                             t2' <- toSemT e t2
                             return (t1' :=: t2', i)
toSemG e i (g1 :/\: g2) = do (g1', j) <- toSemG e i g1
                             (g2', k) <- toSemG e j g2
                             return (g1' :/\: g2', k)
toSemG e i (g1 :\/: g2) = do (g1', j) <- toSemG e i g1
                             (g2', k) <- toSemG e j g2
                             return (g1' :\/: g2', k)
toSemG e i (Invoke n a) = do a' <- toSemTs e a
                             return (Invoke n a', i)
toSemG e i (Fresh n g)  = toSemG ((n, V i) : e) (i + 1) g
toSemG e i _            = Left "Let-expressions aren't supported."

unify :: Subst -> Ts -> Ts -> Maybe Subst
unify (n, s) t1 t2 = Eval.unify (Just s) t1 t2 >>= return . ((,) n)

---------------------------------------

fillHole :: HoleStream l -> a -> Er (GenStream a l)
fillHole (Goal g _  ) a = return $ Goal g a
fillHole (Conj s l g) a = fillHole s a >>= \s -> return $ Conj s l g
fillHole _            _ = Left "Unexpected disjunction in stream with hole."


swapConjs :: Labels l p => p -> Stream l -> HoleStream l -> Er (Stream l)
swapConjs p = swap p id where
  swap :: Labels l p => p -> (HoleStream l -> HoleStream l) -> Stream l -> HoleStream l -> Er (Stream l)
  swap par conjs (Goal g s)   conj = fillHole conj s >>= \s -> return $ Conj s (new par s) $ conjs (Goal g ())
  swap par conjs (Disj p q)   conj = swap par conjs p conj >>= \p -> swap par conjs q conj >>= \q -> return $ Disj p q
  swap par conjs (Conj s l g) conj = swap par (\s -> conjs $ Conj s (keep par l) g) s conj

---------------------------------------

eval :: Labels l p => p -> [Fun] -> Stream l -> Er (Maybe (Stream l), Maybe Subst)
eval par fs (Goal (p :=: q) s)        = return (Nothing, unify s p q)
eval par fs (Goal (p :\/: q) s)       = return (Just $ Disj (Goal p s) $ Goal q s, Nothing)
eval par fs (Goal (p :/\: g) s)       = let p' = Goal p s in
                                        return (Just $ Conj p' (new par p') $ Goal g (), Nothing)
eval _   _  (Goal (Fresh _ _) _)      = Left "Unexpected fresh-expresseion."
eval _   _  (Goal (Let _ _) _)        = Left "Let-expressions aren't supported."
eval par fs (Goal (Invoke n a) (i,s)) =
  case lookup n fs of
    Nothing      -> Left $ printf "Call of undefined relation '%s'." n
    Just (ns, g) ->
      if length ns == length a
        then toSemG (zip ns a) i g >>= \(g,j) -> return $ (Just $ Goal g (j,s), Nothing)
        else Left $ printf
               "Unexpected count of relation's arguments (actual: %d, expected: %d)."
               (length a) (length ns)
eval par fs (Conj s l g) =
  if predicate par s l
    then eval par fs s >>= \r ->
      case r of
        (Nothing, Nothing) -> return r
        (Nothing, Just a ) -> fillHole g a >>= \s -> return (Just s, Nothing)
        (Just s , Nothing) -> return (Just $ Conj s (update par s l) g, Nothing)
        (Just s , Just a ) -> fillHole g a >>= \s' -> return (Just $ Disj s' $ Conj s (update par s l) g, Nothing)
    else swapConjs par s g >>= \s -> return (Just s, Nothing)
eval par fs (Disj p q) = eval par fs p >>= \(s, a) ->
  case s of
    Nothing -> return (Just q, a)
    Just s  -> return (Just $ Disj q s, a)

---------------------------------------

printInfo :: Int -> Int -> Stream l -> a -> a
printInfo step i s =
  if i `mod` step /= 0 then id else
    let dInC = disjsInConjs s in
    trace $ printf "step: %10d\nhigh: %10d\nsize: %10d\ndisj: %10d\nconj: %10d\ncnjA: %10d\nmaxD: %10d\n\n"
            i (high s) (size s) (disjCount s) (conjCount s) (length dInC) (maximum $ (-1):dInC)

def2fun :: Def -> Fun
def2fun (Def n a g) = (n, (a, g))

prepareAnswer :: Subst -> [X] -> [(X, Ts)]
prepareAnswer s x = map (\(x,i) -> (x, getTerm s $ V i)) $ zip x [0..]

data Answers a b = Nil b
                 | a ::: Answers a b

type RunAnswers = Answers ([(X, Ts)], Int) (Maybe String, Int)

instance (Show a, Show b) => Show (Answers a b) where
  show a = printf "answers {\n%s" $ show' a where
    show' :: (Show a, Show b) => Answers a b -> String
    show' (Nil b)    = printf "\nmessage: %s\n}\n" $ show b
    show' (x ::: xs) = printf "\n%s\n%s" (show x) $ show' xs

takeAnswers :: Int -> RunAnswers -> RunAnswers
takeAnswers _ a@(Nil _)        = a 
takeAnswers n _  | n <= 0      = Nil (Just "Zero answers", 0)
takeAnswers 1 (a@(_, i) ::: _) = a ::: Nil (Just "Enough answers.", i)
takeAnswers n (a ::: b)        = a ::: takeAnswers (n-1) b 
    
run :: (Labels l p, Show l) => [X] -> [Def] -> p -> RunGoal X l -> RunAnswers
run vars defs p g =
  case toSemG' (zip vars $ map V [0..]) (length vars) g of
    Left msg     -> Nil (Just $ "Error in initial goal. " ++ msg, 0)
    Right (g, i) -> run' p 0 (map def2fun defs) $ goal g (i, [])
  where
  toSemG' :: [(X, Ts)] -> Int -> RunGoal X l -> Er (RunGoal S l, Int)
  toSemG' e i (RG g) = toSemG e i g >>= \(g,i) -> return (RG g, i)
  goal :: RunGoal S l -> Subst -> Stream l
  goal (RG g) = Goal g
  run' :: (Labels l p, Show l) => p -> Int -> [Fun] -> Stream l -> RunAnswers
  run' p i fs s =
    case eval p fs s of
      Left msg                 -> Nil (Just msg, i)
      Right (Nothing, Nothing) -> Nil (Nothing, i)
      Right (Nothing, Just a ) -> (prepareAnswer a vars, i) ::: Nil (Nothing, i)
      Right (Just s , Nothing) ->
        -- printInfo 10000 i s $
        run' p (i+1) fs s
      Right (Just s , Just a ) ->
        -- printInfo 10000 i s $
        (prepareAnswer a vars, i) ::: run' p (i+1) fs s
