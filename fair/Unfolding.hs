module Unfolding where

import Text.Printf  (printf)
import Data.Maybe   (mapMaybe, fromMaybe)
import Data.List    (find)
-- import Debug.Trace  (trace)

import Syntax

import Eval         (sEmpty)
import FairEval     (Answers((:::), Nil), toSemG, takeAns, def2fun, prepareAnswer)
import FairStream   (Fun, Subst, Er)
import Util         (unify, substInT)
import DefsAnalysis (def2approx)

----------------------------------------------------

type Call  = (Name, [Ts])

data Stream = Disj Stream Stream
            | Conj Subst [Call]

type Separator = Subst -> [Call] -> Int

----------------------------------------------------

instance Show Stream where
  show (Disj a b) = printf "%s\n\n%s" (show a) $ show b
  show (Conj s c) = foldr (\(n,a) acc -> printf "%s(%s)\n%s" n (showArgs s a) acc) "" c where
    showArgs s []     = ""
    showArgs s [a]    = show $ substInT s a
    showArgs s (a:as) = printf "%s, %s" (show $ substInT s a) $ showArgs s as

----------------------------------------------------

step :: Separator -> [Fun] -> Stream -> Er (Maybe Stream, [Subst])
step sep fs (Disj a b) =
  case step sep fs a of
    Left e             -> Left e
    Right (Nothing, x) -> Right (Just b         , x)
    Right (Just c , x) -> Right (Just $ Disj b c, x)
step sep fs (Conj s cs) =
  let (cs1, c : cs2) = splitAt (sep s cs) cs in
  case unfold fs s c of
    Left e                                -> Left e
    Right Nothing                         -> Right (Nothing, [])
    Right (Just s) | null cs1 && null cs2 -> Right $ splitAnswers s
    Right (Just s)                        -> Right (Just $ attachConjs cs1 cs2 s, [])

splitAnswers :: Stream -> (Maybe Stream, [Subst])
splitAnswers (Disj a b) =
  case (splitAnswers a, splitAnswers b) of
    ((Nothing, a1), (Nothing, a2)) -> (Nothing          , a1 ++ a2)
    ((Just s1, a1), (Nothing, a2)) -> (Just s1          , a1 ++ a2)
    ((Nothing, a1), (Just s2, a2)) -> (Just s2          , a1 ++ a2)
    ((Just s1, a1), (Just s2, a2)) -> (Just $ Disj s1 s2, a1 ++ a2)
splitAnswers (Conj a []) = (Nothing, [a])
splitAnswers s           = (Just s, [])

attachConjs :: [Call] -> [Call] -> Stream -> Stream
attachConjs cs1 cs2 (Disj a b)  = Disj (attachConjs cs1 cs2 a) $ attachConjs cs1 cs2 b
attachConjs cs1 cs2 (Conj s cs) = Conj s $ cs1 ++ cs ++ cs2

unfold :: [Fun] -> Subst -> Call -> Er (Maybe Stream)
unfold fs (i, s) (n, a) =
  case lookup n fs of
    Nothing -> Left $ printf "Undefined relation '%s'." n
    Just (x, g) | length x == length a ->
      case toSemG (zip x a) i g of
        Left e       -> Left e
        Right (g, i) -> Right $ goalToStream (i, s) g
    Just (x, _) -> Left $ printf "Unexpected count of arguments (relation: '%s', expected: %d, actual: %d)" n (length x) $ length a
  where

goalToStream :: Subst -> G S -> Maybe Stream
goalToStream s g = g2s (Conj s []) g where
  disjCmb :: Maybe Stream -> Maybe Stream -> Maybe Stream
  disjCmb Nothing  b        = b
  disjCmb a        Nothing  = a
  disjCmb (Just a) (Just b) = Just $ Disj a b
  g2s :: Stream -> G S -> Maybe Stream
  g2s (Disj a b)  g            = disjCmb (g2s a g) $ g2s b g
  g2s (Conj s cs) (t1 :=: t2)  = unify s t1 t2 >>= \s -> return $ Conj s cs
  g2s (Conj s cs) (Invoke n a) = return $ Conj s $ cs ++ [(n, a)]
  g2s s           (g1 :/\: g2) = g2s s g1 >>= \s -> g2s s g2
  g2s s           (g1 :\/: g2) = disjCmb (g2s s g1) $ g2s s g2

----------------------------------------------------

type RunAnswers =  Answers ([(X, Ts)], Int) (Maybe String, Int)

takeAnswers :: Int -> RunAnswers -> RunAnswers
takeAnswers = takeAns 0

prepareStream :: [X] -> G X -> Er (Maybe Stream)
prepareStream vars g =
  case toSemG (zip vars $ map V [0..]) (length vars) g of
    Left e       -> Left $ printf "Error in initial goal. %s" e
    Right (g, i) -> Right $ goalToStream (i, sEmpty) g

run :: Separator -> [X] -> [Def] -> G X -> RunAnswers
run sep vars defs g =
  case prepareStream vars g of
    Left e         -> Nil (Just e, 0)
    Right Nothing  -> Nil (Nothing, 0)
    Right (Just s) -> run' 1 s
  where
    funs :: [Fun]
    funs = map def2fun defs
    run' :: Int -> Stream -> RunAnswers
    run' i s =
      case step sep funs s of
        Left e             -> Nil (Just e, i)
        Right (Nothing, a) -> foldr (\s acc -> (prepareAnswer s vars, i) ::: acc) (Nil (Nothing, i)) a
        Right (Just s , a) -> foldr (\s acc -> (prepareAnswer s vars, i) ::: acc) (run' (i+1) s) a

run1 :: Separator -> [X] -> [Def] -> G X -> (Er [(X, Ts)], Int)
run1 sep vars defs g =
  case prepareStream vars g of
    Left e         -> (Left e, 0)
    Right Nothing  -> (Left "Zero answers", 0)
    Right (Just s) -> run1' 1 s
  where
    funs :: [Fun]
    funs = map def2fun defs
    run1' :: Int -> Stream -> (Er [(X, Ts)], Int)
    run1' i s =
      case step sep funs s of
        Left e             -> (Left e, i)
        Right (_,  (x:xs)) -> (Right $ prepareAnswer x vars, i)
        Right (Nothing, _) -> (Left "Zero answers", i)
        Right (Just s , _) -> run1' (i+1) s

----------------------------------------------------

simpleSep :: Separator
simpleSep _ _ = 0

defsRatingSep :: [Def] -> Separator
defsRatingSep ds s cs = indexOfMin 0 0 1 $ map getIndex cs where
  aps = map def2approx ds
  indexOfMin :: Int -> Int -> Double -> [Double] -> Int
  indexOfMin _ j _ []             = j
  indexOfMin i j a (x:xs) | x < a = indexOfMin (i+1) i x xs
  indexOfMin i j a (x:xs)         = indexOfMin (i+1) j a xs
  getIndex :: Call -> Double
  getIndex (n, a) =
    case lookup n aps of
      Nothing    -> error $ printf "Undefined relation:'%s'." n
      Just cases ->
        let l1 = length cases in
        let l2 = length $ mapMaybe (\c -> foldl (\acc (t,u) -> acc >>= \s -> unify s t u) (Just s) $ zip a c) cases in
        fromIntegral l2 / fromIntegral l1

firstGoodCallSep :: [Def] -> Separator
firstGoodCallSep ds s cs = fromMaybe 0 $ fmap fst $ find (isGood . snd) $ zip [0..] cs where
  aps = map def2approx ds
  isGood :: Call -> Bool
  isGood (n, a) =
    case lookup n aps of
      Nothing    -> error $ printf "Undefined relation:'%s'." n
      Just cases ->
        let l1 = length cases in
        let l2 = length $ mapMaybe (\c -> foldl (\acc (t,u) -> acc >>= \s -> unify s t u) (Just s) $ zip a c) cases in
        l1 <= 1 || l1 > l2
