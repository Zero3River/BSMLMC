

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
(|=) :: ModelState -> Bool
(|=) = undefined

(=|) :: ModelState -> Bool
(=|) = undefined
\end{code}