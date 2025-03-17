

\section{The Definiton of Model Checker Data Type}\label{sec:Basics}

The following is the definition of our Data Type for Model Checker.

\begin{code}
-- {-# LANGUAGE InstanceSigs #-}
module Checker where



import Control.Monad
import System.Random
import Test.QuickCheck
import Data.List

import Syntax


type World = Integer
type Universe = [World]
type Proposition = Int
type State = [World]

type Valuation = World -> [Proposition]
type Relation = [(World,World)]

data KripkeModel = KrM Universe Valuation Relation

type ModelState = (KripkeModel,State)

instance Show KripkeModel where
  show (KrM u v r) = "KrM " ++ show u ++ " " ++ vstr ++ " " ++ show r where
    vstr = "(fromJust . flip lookup " ++ show [(w, v w) | w <- u] ++ ")"

\end{code}

The following helper function defines the set of all successors of a world:

\begin{code}
(!) :: Relation -> World -> [World]
(!) r w = map snd $ filter ((==) w . fst) r
\end{code}

Here we define the semantics of \textbf{BSML} ...

\begin{code}

-- helper function to find all pairs of worlds t and u that the union of t and u is the input s
allPairs :: [World] -> [([World], [World])]
allPairs []     = [([],[])]
allPairs (x:xs) =
  [ (x:ts, x:us) | (ts,us) <- allPairs xs ] ++  
  [ (x:ts, us)   | (ts,us) <- allPairs xs ] ++  
  [ (ts, x:us)   | (ts,us) <- allPairs xs ]  

(|=) :: ModelState -> BSMLForm -> Bool
(KrM _ v _, s) |= (P p) = all (\w -> p `elem` v w) s
(_, s) |= Bot = null s
(_, s) |= NE = not $ null s
(KrM u v r, s) |= (Neg f) = (KrM u v r, s) =| f
m |= (Con f g) = m |= f && m |= g
(k, s) |= (Dis f g) = any (\(ts,us) -> (k, ts) |= f && (k, us) |= g) (allPairs s)
m |= (Gdis f g) = m |= f || m |= g
(KrM u v r, s) |= (Dia f) = undefined


(=|) :: ModelState -> BSMLForm -> Bool
(KrM _ v _, s) =| (P p) = all (\w -> p `notElem` v w) s
(_, _) =| Bot = True
(_, s) =| NE = null s
(KrM u v r, s) =| (Neg f) = (KrM u v r, s) |= f
(k, s) =| (Con f g) = any (\(ts,us) -> (k, ts) =| f && (k, us) =| g) (allPairs s)
m =| (Dis f g) = m =| f && m =| g
m =| (Gdis f g) = m =| f && m =| g
(KrM u v r, s) =| (Dia f) = undefined

\end{code}