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
  
instance (Show s, Show l) => Show (GenStream s l) where
  show (Goal e g s) = printf "<%s, %s, %s>" (show e) (show g) $ show s
  show (Disj p q)   = printf "(%s |+| %s)" (show p) $ show q
  show (Conj s l g) = printf "(%s |*|{%s} %s)" (show s) (show l) $ show g
  
---------------------------------------

class Labels a where
  new       :: Stream a -> a
  predicate :: Stream a -> a -> Bool
  update    :: Stream a -> a -> a
  
instance Labels () where
  new       _   = ()
  predicate _ _ = False
  update    _ _ = ()
  
instance Labels Int where
  new       _   = 100
  predicate _ n = n == 0
  update    _ n = n - 1

data Disj = D Int Int deriving Show

disjs (Conj _ (D d _) _) = d
disjs (Disj a b)         = 1 + disjs a + disjs b
disjs _                  = 0

instance Labels Disj where
  new       s         = D (disjs s) 10000
  predicate _ (D d n) = n == 0 || d >= 100
  update    s (D _ n) = D (disjs s) (n - 1)
  
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

 
swapConjs :: Labels l => Stream l -> HoleStream l -> Er (Stream l)
swapConjs = swap id where
  swap :: Labels l => (HoleStream l -> HoleStream l) -> Stream l -> HoleStream l -> Er (Stream l)
  swap conjs (Goal e g s) conj = fillHole conj s >>= \s -> return $ Conj s (new s) $ conjs (Goal e g ())
  swap conjs (Disj p q)   conj = swap conjs p conj >>= \p -> swap conjs q conj >>= \q -> return $ Disj p q
  swap conjs (Conj s l g) conj = swap (\s -> conjs $ Conj s l g) s conj

---------------------------------------

eval :: Labels l => [Fun] -> Stream l -> Er (Maybe (Stream l), Maybe Subst)
eval fs (Goal e (p :=: q) s)       = unify e s p q >>= return . (,) Nothing
eval fs (Goal e (p :\/: q) s)      = return (Just $ Disj (Goal e p s) $ Goal e q s, Nothing)
eval fs (Goal e (p :/\: g) s)      = let p' = Goal e p s in 
                                     return (Just $ Conj p' (new p') $ Goal e g (), Nothing)
eval fs (Goal e (Fresh n g) (i,s)) = return (Just $ Goal ((n, V i) : e) g (i+1,s), Nothing)
eval fs (Goal e (Invoke n a) s)    = 
  case lookup n fs of
    Nothing      -> Left $ printf "Call of undefined relation '%s'." n
    Just (ns, g) -> 
      if length ns == length a 
        then toSemList e a >>= \a -> return $ (Just $ Goal (zip ns a ++ e) g s, Nothing)
        else Left $ printf 
               "Unexpected count of relation's arguments (actual: %d, expected: %d)." 
               (length a) (length ns)
eval fs (Goal e (Let (Def n a g) g') s) = Left "Let-expressions isn't supported."
eval fs (Conj s l g) | predicate s l = swapConjs s g >>= \s -> return (Just s, Nothing)
eval fs (Conj s l g) = eval fs s >>= \r ->
  case r of
    (Nothing, Nothing) -> return r
    (Nothing, Just a ) -> fillHole g a >>= \s -> return (Just s, Nothing)
    (Just s , Nothing) -> return (Just $ Conj s (update s l) g, Nothing)
    (Just s , Just a ) -> fillHole g a >>= \s' -> return (Just $ Disj s' $ Conj s (update s l) g, Nothing)
eval fs (Disj p q) = eval fs p >>= \(s, a) ->
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

disjsInConjs :: GenStream a b -> [Int]
disjsInConjs (Conj a _ _) = [disjCount a]
disjsInConjs (Disj a b)   = disjsInConjs a ++ disjsInConjs b
disjsInConjs _            = []

---------------------------------------

def2fun :: Def -> Fun
def2fun (Def n a g) = (n, (a, g))

initialState :: Labels l => G X -> Stream l
initialState g = Goal [] g (0, [])

getTerm :: Subst -> Ts -> Ts
getTerm s (V x) = 
  case lookup x $ snd s of
    Nothing -> V x
    Just t  -> getTerm s t
getTerm s (C n a) = C n $ map (getTerm s) a

run :: (Labels l, Show l) => Int -> Int -> [Def] -> Stream l -> Er ([(Ts, Int)], Int)
run n q = run' 0 n . map def2fun where 
  run' i 0 _  _ = return ([], i)
  run' i n fs s = eval fs s >>= \r -> 
    case r of
      (Nothing, Nothing) -> return ([], i)
      (Nothing, Just a ) -> return ([(getTerm a $ V q, i)], i)
      (Just s , Nothing) -> 
        --(if mod i 10000 == 0 then traceShow (i, high s, size s, disjCount s, maximum (0 : disjsInConjs s)) . trace "\n\n\n" else id) $
        run' (i+1) n fs s
      (Just s , Just a ) -> 
        --(if mod i 10000 == 0 then traceShow (i, high s, size s, disjCount s, maximum (0 : disjsInConjs s)) . trace "\n\n\n" else id) $
        run' (i+1) (n-1) fs s >>= \(xs, j) -> return $ ((getTerm a $ V q, i) : xs, j)



