{-# LANGUAGE MultiParamTypeClasses #-}

module Fair where

import Text.Printf
import Debug.Trace

import Syntax
import qualified Eval
import qualified Embed
---------------------------------------

type Er a = Either String a

type Goal  = G S
type Subst = (S, [(S, Ts)])
type Hole  = ()
type Fun   = (Name, ([Name], G X))

data GenStream subst label
  = Goal Goal subst
  | Disj (GenStream subst label) (GenStream subst label)
  | Conj (GenStream subst label) label (GenStream Hole label)

type HoleStream l = GenStream Hole l
type Stream     l = GenStream Subst l

type InitialStream l = Int -> Stream l

instance (Show s, Show l) => Show (GenStream s l) where
  show (Goal g s) = printf "<%s, %s>" (show g) $ show s
  show (Disj p q)   = printf "(%s |+| %s)" (show p) $ show q
  show (Conj s l g) = printf "(%s |*|{%s} %s)" (show s) (show l) $ show g

---------------------------------------

class Labels l p where
  new       :: p -> Stream l -> l
  keep      :: p -> l -> l
  predicate :: p -> Stream l -> l -> Bool
  update    :: p -> Stream l -> l -> l

instance Labels () () where
  new       () _    = ()
  keep      ()   () = ()
  predicate () _ () = True
  update    () _ () = ()

instance Labels Int Int where
  new       i _   = i
  keep      _   n = n
  predicate _ _ n = n /= 0
  update    _ _ n = n - 1

data Disj = D Int Int deriving Show

disjs (Conj _ (D d _) _) = d
disjs (Disj a b)         = 1 + disjs a + disjs b
disjs _                  = 0

instance Labels Disj Disj where
  new       (D _ i) s         = D (disjs s) i
  keep      _         (D _ n) = D 0 n
  predicate (D p _) _ (D d n) = n /= 0 && d <= p
  update    _       s (D _ n) = D (disjs s) (n - 1)

data SignVars  = SV [(Int, Ts)] Int Int deriving Show
data SignVarsP = SVP [Int] Int Int

-- It desn't work
instance Labels SignVars SignVarsP where
  new (SVP l n m) s = SV (map (\v -> (v, getTerm a $ V v)) l) n m where
    (_, a) = getLeftLeaf s
  keep _ l = l
  predicate _ s (SV v i j) = j /= 0 && (i /= 0 || any (\(i, t) -> Embed.isStrictInst t $ getTerm a $ V i) v) where
    (_, a) = getLeftLeaf s
  update (SVP l n _) s (SV _ 0 m) = SV (map (\v -> (v, getTerm a $ V v)) l) n m where
    (_, a) = getLeftLeaf s
  update _ _ (SV l n m) = SV l (n-1) (m-1)

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

getLeftLeaf :: GenStream s l -> (Goal, s)
getLeftLeaf (Disj a _  ) = getLeftLeaf a
getLeftLeaf (Conj a _ _) = getLeftLeaf a
getLeftLeaf (Goal g s) = (g, s)

high :: GenStream a b -> Int
high (Conj a _ _) = 1 + high a
high (Disj a _  ) = 1 + high a
high _            = 0

size :: GenStream a b -> Int
size (Conj a _ b) = 1 + size a + size b
size (Disj a b)   = 1 + size a + size b
size _            = 1

disjCount :: GenStream a b -> Int
disjCount (Conj a _ _) = disjCount a
disjCount (Disj a b)   = 1 + disjCount a + disjCount b
disjCount _            = 0

conjCount :: GenStream a b -> Int
conjCount (Conj a _ b) = 1 + conjCount a + conjCount b
conjCount (Disj a b)   = conjCount a + conjCount b
conjCount _            = 0

disjsInConjs :: GenStream a b -> [Int]
disjsInConjs (Conj a _ _) = [disjCount a]
disjsInConjs (Disj a b)   = disjsInConjs a ++ disjsInConjs b
disjsInConjs _            = []

---------------------------------------

printInfo :: Int -> Int -> Stream l -> a -> a
printInfo step i s =
  if i `mod` step /= 0 then id else
    let dInC = disjsInConjs s in
    trace $ printf "step: %10d\nhigh: %10d\nsize: %10d\ndisj: %10d\nconj: %10d\ncnjA: %10d\nmaxD: %10d\n\n"
            i (high s) (size s) (disjCount s) (conjCount s) (length dInC) (maximum $ (-1):dInC)

def2fun :: Def -> Fun
def2fun (Def n a g) = (n, (a, g))

initialState :: Goal -> InitialStream l
initialState g i = Goal g (i, [])

getTerm :: Subst -> Ts -> Ts
getTerm s (V x) =
  case lookup x $ snd s of
    Nothing -> V x
    Just t  -> getTerm s t
getTerm s (C n a) = C n $ map (getTerm s) a

prepareAnswer :: Subst -> [S] -> [(S, Ts)]
prepareAnswer s = map $ \i -> (i, getTerm s $ V i)

run :: (Labels l p, Show l) => Int -> [S] -> [Def] -> p -> InitialStream l -> Er ([([(S, Ts)], Int)], Int)
run n vars defs p gGen = run' p 0 n (map def2fun defs) (gGen $ length vars) where
  run' :: (Labels l p, Show l) => p -> Int -> Int -> [Fun] -> Stream l -> Er ([([(S, Ts)], Int)], Int)
  run' _ i 0 _  _ = return ([], i)
  run' p i n fs s = eval p fs s >>= \r ->
    case r of
      (Nothing, Nothing) -> return ([], i)
      (Nothing, Just a ) -> return ([(prepareAnswer a vars, i)], i)
      (Just s , Nothing) ->
        printInfo 10000 i s $
        run' p (i+1) n fs s
      (Just s , Just a ) ->
        printInfo 10000 i s $
        run' p (i+1) (n-1) fs s >>= \(xs, j) -> return $ ((prepareAnswer a vars, i) : xs, j)
