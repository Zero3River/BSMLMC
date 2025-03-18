

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

-- helper function to find all non-empty subsets of a list
subsetsNonEmpty :: [World] -> [[World]]
subsetsNonEmpty [] = [] 
subsetsNonEmpty (x:xs) =
  let rest = subsetsNonEmpty xs
  in [[x]] ++ rest ++ map (x:) rest

(|=) :: ModelState -> BSMLForm -> Bool
(KrM _ v _, s) |= (P p) = all (\w -> p `elem` v w) s
(_, s) |= Bot = null s
(_, s) |= NE = not $ null s
(KrM u v r, s) |= (Neg f) = (KrM u v r, s) =| f
m |= (Con f g) = m |= f && m |= g
(k, s) |= (Dis f g) = any (\(ts,us) -> (k, ts) |= f && (k, us) |= g) (allPairs s)
m |= (Gdis f g) = m |= f || m |= g
(KrM u v r, s) |= (Dia f) = all (\w -> any (\l -> (KrM u v r,l) |= f ) (subsetsNonEmpty (r ! w)))  s


(=|) :: ModelState -> BSMLForm -> Bool
(KrM _ v _, s) =| (P p) = all (\w -> p `notElem` v w) s
(_, _) =| Bot = True
(_, s) =| NE = null s
(KrM u v r, s) =| (Neg f) = (KrM u v r, s) |= f
(k, s) =| (Con f g) = any (\(ts,us) -> (k, ts) =| f && (k, us) =| g) (allPairs s)
m =| (Dis f g) = m =| f && m =| g
m =| (Gdis f g) = m =| f && m =| g
(KrM u v r, s)  =| (Dia f) = all (\w -> (KrM u v r, r ! w) =| f)  s
\end{code}


A model state pair $(M,s)$ is indisputable if for all  $w,v \in s, R[w] = R[v]$. 


\begin{code}
indisputable :: ModelState -> Bool
indisputable (KrM _ _ r ,s) = any (\w -> any (\v -> sort (r ! w) == sort (r ! v )) s ) s
\end{code}


A model state pair is state-based if for all $w \in s, R[w] = s$.


\begin{code}
stateBased :: ModelState -> Bool
stateBased (KrM _ _ r , s) = any (\w -> sort (r ! w) == sort s) s
\end{code}
\begin{code}
example1 :: KripkeModel
example1 = KrM [0,1,2] myVal [(0,1), (1,2), (2,1)] where
  myVal 0 = [0]
  myVal _ = [4]

example2 :: KripkeModel
example2 = KrM [0,1] myVal [(0,1), (1,1)] where
  myVal 0 = [0]
  myVal _ = [0, 4]

example11 :: ModelState
example11 = (example1, [0,1,2])

example12 :: ModelState
example12 = (example2, [0,1])


\end{code}