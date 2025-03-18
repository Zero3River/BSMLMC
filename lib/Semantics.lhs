

\section{Semantics}\label{sec:Semantics}


The semantics of BSML is based on \textit{team semantics}, where formulas are interpreted with respect to sets of possible worlds (called \textit{states}) rather than single worlds. A \textit{model} \(M\) is a triple \((W, R, V)\), where:
\begin{itemize}
    \item \(W\) is a nonempty set of possible worlds,
    \item \(R \subseteq W \times W\) is an accessibility relation,
    \item \(V: \text{Prop} \to \mathcal{P}(W)\) is a valuation function.
\end{itemize}

A \textit{state} \(s\) is a subset of \(W\). The support and anti-support conditions for BSML formulas are defined recursively as follows:

\begin{align*}
M, s &\models p \quad \text{iff} \quad \forall w \in s, w \in V(p) \\
M, s &\leftmodels p \quad \text{iff} \quad \forall w \in s, w \notin V(p) \\
M, s &\models \bot \quad \text{iff} \quad s = \emptyset \\
M, s &\leftmodels \bot \quad \text{always} \\
M, s &\models \text{NE} \quad \text{iff} \quad s \neq \emptyset \\
M, s &\leftmodels  \text{NE} \quad \text{iff} \quad s = \emptyset \\
M, s &\models \neg \varphi \quad \text{iff} \quad M, s \leftmodels \varphi \\
M, s &\leftmodels  \neg \varphi \quad \text{iff} \quad M, s \models \varphi \\
M, s &\models \varphi \land \psi \quad \text{iff} \quad M, s \models \varphi \text{ and } M, s \models \psi \\
M, s &\leftmodels  \varphi \land \psi \quad \text{iff} \quad \exists t, u \subseteq s \text{ s.t. } s = t \cup u \text{ and } M, t \leftmodels \varphi \text{ and } M, u \leftmodels \psi \\
M, s &\models \varphi \lor \psi \quad \text{iff} \quad \exists t, u \subseteq s \text{ s.t. } s = t \cup u \text{ and } M, t \models \varphi \text{ and } M, u \models \psi \\
M, s &\leftmodels \varphi \lor \psi \quad \text{iff} \quad M, s \leftmodels \varphi \text{ and } M, s \leftmodels \psi \\
M, s &\models \varphi \inqdisj \psi \quad \text{iff} \quad M, s \models \varphi \text{ or } M, s \models \psi \\
M, s &\leftmodels \varphi \inqdisj \psi \quad \text{iff} \quad M, s \leftmodels\varphi \text{ and } M, s \leftmodels \psi\\
M, s &\models \Diamond \varphi \quad \text{iff} \quad \forall w \in s, \exists t \subseteq R[w] \text{ s.t. } t \neq \emptyset \text{ and } M, t \models \varphi \\
M, s &\leftmodels  \Diamond \varphi \quad \text{iff} \quad \forall w \in s, M, R[w] \leftmodels \varphi
\end{align*}


The box modality \(\Box\) is defined as the dual of the \(\Diamond\), meaning \(\Box \varphi\) is equivalent to \(\neg \Diamond \neg \varphi\). This leads to the following support and antisupport clauses:

\begin{align*}
M, s &\models \Box \varphi \quad \text{iff} \quad \forall w \in s, M, R[w] \models \varphi \\
M, s &\leftmodels  \Box \varphi \quad \text{iff} \quad  \forall w \in s, \exists t \subseteq R[w] \text{ s.t. } t \neq \emptyset \text{ and } M, t \leftmodels \varphi 
\end{align*}



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