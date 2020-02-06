{-# LANGUAGE MultiParamTypeClasses #-}

module Fair where

import Text.Printf
import Debug.Trace

import Syntax
import qualified Eval as E
---------------------------------------

type Er a = Either String a

type Goal  = G X
type Env   = [(X, Ts)]
type Subst = (S, [(S, Ts)])
type Hole  = ()
type Fun   = (Name, ([Name], G X))

data GenStream subst label
  = Goal Env Goal subst
  | Disj (GenStream subst label) (GenStream subst label)
  | Conj (GenStream subst label) label (GenStream Hole label)

type HoleStream l = GenStream Hole l
type Stream     l = GenStream Subst l

type InitialStream l = Env -> Int -> Stream l

instance (Show s, Show l) => Show (GenStream s l) where
  show (Goal e g s) = printf "<%s, %s, %s>" (show e) (show g) $ show s
  show (Disj p q)   = printf "(%s |+| %s)" (show p) $ show q
  show (Conj s l g) = printf "(%s |*|{%s} %s)" (show s) (show l) $ show g

---------------------------------------

class Labels l p where
  new       :: p -> Stream l -> l
  keep      :: p -> l -> l
  predicate :: p -> Stream l -> l -> Bool
  update    :: p -> Stream l -> l -> l

instance Labels () () where
  new       _ _   = ()
  keep      _ _   = ()
  predicate _ _ _ = False
  update    _ _ _ = ()

instance Labels Int Int where
  new       i _   = i
  keep      _ n   = n
  predicate _ _ n = n == 0
  update    _ _ n = n - 1

data Disj = D Int Int deriving Show

disjs (Conj _ (D d _) _) = d
disjs (Disj a b)         = 1 + disjs a + disjs b
disjs _                  = 0

instance Labels Disj Disj where
  new       (D _ i) s         = D (disjs s) i
  keep      _         (D _ n) = D 0 n
  predicate (D p _) _ (D d n) = n == 0 || d >= p
  update    _       s (D _ n) = D (disjs s) (n - 1)

---------------------------------------

toSemList :: Env -> [Tx] -> Er [Ts]
toSemList e l = foldr combine (return []) l where
  combine x l = toSem e x >>= \x -> l >>= \l -> return $ x : l

toSem :: Env -> Tx -> Er Ts
toSem e (V x) = case lookup x e of
    Nothing -> Left $ printf "Unexpected variable '%s'." x
    Just x  -> return x
toSem e (C n a) = toSemList e a >>= return . (C n)


unify :: Env -> Subst -> Tx -> Tx -> Er (Maybe Subst)
unify e (n, s) t1 t2 =
  do
    t1 <- toSem e t1
    t2 <- toSem e t2
    return $ E.unify (Just s) t1 t2 >>= return . ((,) n)

---------------------------------------

fillHole :: HoleStream l -> a -> Er (GenStream a l)
fillHole (Goal e g _) a = return $ Goal e g a
fillHole (Conj s l g) a = fillHole s a >>= \s -> return $ Conj s l g
fillHole _            _ = Left "Unexpected disjunction in stream with hole."


swapConjs :: Labels l p => p -> Stream l -> HoleStream l -> Er (Stream l)
swapConjs p = swap p id where
  swap :: Labels l p => p -> (HoleStream l -> HoleStream l) -> Stream l -> HoleStream l -> Er (Stream l)
  swap par conjs (Goal e g s) conj = fillHole conj s >>= \s -> return $ Conj s (new par s) $ conjs (Goal e g ())
  swap par conjs (Disj p q)   conj = swap par conjs p conj >>= \p -> swap par conjs q conj >>= \q -> return $ Disj p q
  swap par conjs (Conj s l g) conj = swap par (\s -> conjs $ Conj s (keep par l) g) s conj

---------------------------------------

eval :: Labels l p => p -> [Fun] -> Stream l -> Er (Maybe (Stream l), Maybe Subst)
eval par fs (Goal e (p :=: q) s)       = unify e s p q >>= return . (,) Nothing
eval par fs (Goal e (p :\/: q) s)      = return (Just $ Disj (Goal e p s) $ Goal e q s, Nothing)
eval par fs (Goal e (p :/\: g) s)      = let p' = Goal e p s in
                                         return (Just $ Conj p' (new par p') $ Goal e g (), Nothing)
eval par fs (Goal e (Fresh n g) (i,s)) = return (Just $ Goal ((n, V i) : e) g (i+1,s), Nothing)
eval par fs (Goal e (Invoke n a) s)    =
  case lookup n fs of
    Nothing      -> Left $ printf "Call of undefined relation '%s'." n
    Just (ns, g) ->
      if length ns == length a
        then toSemList e a >>= \a -> return $ (Just $ Goal (zip ns a ++ e) g s, Nothing)
        else Left $ printf
               "Unexpected count of relation's arguments (actual: %d, expected: %d)."
               (length a) (length ns)
eval par fs (Goal e (Let (Def n a g) g') s) = Left "Let-expressions isn't supported."
eval par fs (Conj s l g) | predicate par s l = swapConjs par s g >>= \s -> return (Just s, Nothing)
eval par fs (Conj s l g) = eval par fs s >>= \r ->
  case r of
    (Nothing, Nothing) -> return r
    (Nothing, Just a ) -> fillHole g a >>= \s -> return (Just s, Nothing)
    (Just s , Nothing) -> return (Just $ Conj s (update par s l) g, Nothing)
    (Just s , Just a ) -> fillHole g a >>= \s' -> return (Just $ Disj s' $ Conj s (update par s l) g, Nothing)
eval par fs (Disj p q) = eval par fs p >>= \(s, a) ->
  case s of
    Nothing -> return (Just q, a)
    Just s  -> return (Just $ Disj q s, a)


---------------------------------------

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

initialState :: G X -> InitialStream l
initialState g e i = Goal e g (i, [])

getTerm :: Subst -> Ts -> Ts
getTerm s (V x) =
  case lookup x $ snd s of
    Nothing -> V x
    Just t  -> getTerm s t
getTerm s (C n a) = C n $ map (getTerm s) a

prepareAnswer :: Subst -> [X] -> [(X, Ts)]
prepareAnswer s x = map (\(i,x) -> (x, getTerm s $ V i)) $ zip [0..] x


run :: (Labels l p, Show l) => Int -> [X] -> [Def] -> p -> InitialStream l -> Er ([([(X, Ts)], Int)], Int)
run n vars defs p gGen = run' p 0 n (map def2fun defs) (gGen env count) where
  count = length vars
  env   = zip vars $ map V [0..]
  run' :: (Labels l p, Show l) => p -> Int -> Int -> [Fun] -> Stream l -> Er ([([(X, Ts)], Int)], Int)
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
