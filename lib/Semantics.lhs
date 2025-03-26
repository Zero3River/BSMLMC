


\section{Semantics}\label{sec:Semantics}


The semantics of BSML is based on \textit{team semantics}, where formulas are interpreted with respect to sets of possible worlds, which called \textit{states} or \textit{teams} rather than single worlds. A \textit{model} \(M\) is a triple \((W, R, V)\), where:

\begin{itemize}
    \item \(W\) is a nonempty set of possible worlds,
    \item \(R \subseteq W \times W\) is an accessibility relation,
    \item \(V: \text{Prop} \to \mathcal{P}(W)\) is a valuation function.
\end{itemize}

A \textit{state} \(s\) is a subset of \(W\), interpreted as an information state. The semantics of $\BSML$ is bilateral, with separate conditions for support $(\models)$, meaning assertion, and anti-support $(\leftmodels)$, meaning rejection. The conditions are defined recursively as follows:

\[
\begin{array}{l c l}
M, s \models p & \text{iff} & \forall w \in s, w \in V(p) \\
M, s \leftmodels p & \text{iff} & \forall w \in s, w \notin V(p) \\
\\
M, s \models \bot & \text{iff} & s = \emptyset \\
M, s \leftmodels \bot & & \text{always} \\
\\
%M, s \models \text{NE} & \text{iff} & s \neq \emptyset \\
%M, s \leftmodels  \text{NE} & \text{iff} & s = \emptyset \\
M, s \models \neg \varphi & \text{iff} & M, s \leftmodels \varphi \\
M, s \leftmodels  \neg \varphi & \text{iff} & M, s \models \varphi \\
\end{array}
\]

An atomic proposition is supported at a state if it holds at every world of that state, and is anti-supported if every world falsifies it. From this, we can already see that $\leftmodels$ is not the same as $\not \models$. This extends to the clauses for falsum $\bot$. Negation is then defined in terms of anti-support. 
\[
\begin{array}{l c l}
M, s \models \varphi \land \psi & \text{iff} & M, s \models \varphi \text{ and } M, s \models \psi \\
M, s \leftmodels  \varphi \land \psi & \text{iff} & \exists t, u \subseteq s \text{ s.t. } s = t \cup u \text{ and } M, t \leftmodels \varphi \text{ and } M, u \leftmodels \psi \\
M, s \models \varphi \lor \psi & \text{iff} & \exists t, u \subseteq s \text{ s.t. } s = t \cup u \text{ and } M, t \models \varphi \text{ and } M, u \models \psi \\
M, s \leftmodels \varphi \lor \psi & \text{iff} & M, s \leftmodels \varphi \text{ and } M, s \leftmodels \psi
\end{array}
\]
A state supports a disjunction if it is a union of two substates, each supporting one of the disjuncts. The intuition is that a disjunction is supported when the information state contains evidence for both of the disjuncts. This is also known as \textit{split disjunction}, since it requires splitting the state into substates. Dually, a conjunction is anti-suported when there is evidence falsifying both of the conjuncts.
\[
\begin{array}{l c l}
M, s \models \Diamond \varphi & \text{iff} & \forall w \in s, \exists t \subseteq R[w] \text{ s.t. } t \neq \emptyset \text{ and } M, t \models \varphi \\
M, s \leftmodels  \Diamond \varphi & \text{iff} & \forall w \in s, \ M, R[w] \leftmodels \varphi \\
\\
M, s \models \Box \varphi & \text{iff} & \forall w \in s, \ M, R[w] \models \varphi \\
M, s \leftmodels  \Box \varphi & \text{iff} & \forall w \in s, \exists t \subseteq R[w] \text{ s.t. } t \neq \emptyset \text{ and } M, t \leftmodels \varphi 
\end{array}
\]
A state supports a diamond formula $\Diamond \varphi$ if each word sees a (non-empty) state supporting $\varphi$; A state anti-supports $\Diamond \varphi$ if all worlds accessible from the state together anti-supports $\varphi$. The clauses for $\Box$ are derived from the clauses for $\neg$ and $\Diamond$. 
\[
\begin{array}{l c l}
M, s \models \text{NE} & \text{iff} & s \neq \emptyset \\
M, s \leftmodels  \text{NE} & \text{iff} & s = \emptyset
\end{array}
\]
As the name suggests, the nonemptiness atom is supported when the state is empty, and is rejected otherwise. When combined with disjunction in the form of pragmatic enrichment, a state supports an enriched disjunction $[\varphi \vee \psi]^+$ when it is a union of two \textit{non-empty}substates, each supporting one of the disjuncts. This is what enables $\BSML$ to derive FC inferences.
\[
\begin{array}{l c l}
M, s &\models \varphi \inqdisj \psi \quad \text{iff} \quad M, s \models \varphi \text{ or } M, s \models \psi \\
M, s &\leftmodels \varphi \inqdisj \psi \quad \text{iff} \quad M, s \leftmodels\varphi \text{ and } M, s \leftmodels \psi\\
\end{array}
\]
more to add...

The following is the definition of our Data Type for Model Checker.


\begin{code}
-- Based on the Homework

-- {-# LANGUAGE InstanceSigs #-}
module Semantics where



import Control.Monad
import System.Random
import Test.QuickCheck
import Data.List

import Syntax
import Control.Lens (below)


type World = Integer
type Universe = [World]
type Proposition = Int
type State = [World]

type Valuation = World -> [Proposition]
type Relation = [(World,World)]

data KripkeModel = KrM Universe Valuation Relation

data ModelState = MS {model :: KripkeModel
                    , state :: State}

instance Show KripkeModel where
  show (KrM u v r) = "KrM " ++ show u ++ " " ++ vstr ++ " " ++ show r where
    vstr = "(fromJust . flip lookup " ++ show [(w, v w) | w <- u] ++ ")"

instance Show ModelState where
  show (MS k s) = "MS " ++ show k ++ " " ++ show s

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
(MS (KrM _ v _) s) |= (P p) = all (\w -> p `elem` v w) s
(MS _ s) |= Bot = null s
(MS _ s) |= NE = not $ null s
(MS (KrM u v r) s) |= (Neg f) = (MS (KrM u v r) s) =| f
m |= (Con f g) = m |= f && m |= g
(MS k s) |= (Dis f g) = any (\(ts,us) -> (MS k ts) |= f && (MS k us) |= g) (allPairs s)
m |= (Gdis f g) = m |= f || m |= g
(MS (KrM u v r) s) |= (Dia f) = all (\w -> any (\l -> (MS (KrM u v r) l) |= f ) (subsetsNonEmpty (r ! w)))  s



(=|) :: ModelState -> BSMLForm -> Bool
(MS (KrM _ v _) s) =| (P p) = all (\w -> p `notElem` v w) s
(MS _ _) =| Bot = True
(MS _ s) =| NE = null s
(MS (KrM u v r) s) =| (Neg f) = (MS (KrM u v r) s) |= f
(MS k s) =| (Con f g) = any (\(ts,us) -> (MS k ts) =| f && (MS k us) =| g) (allPairs s)
m =| (Dis f g) = m =| f && m =| g
m =| (Gdis f g) = m =| f && m =| g
(MS (KrM u v r) s)  =| (Dia f) = all (\w -> (MS (KrM u v r) (r ! w)) =| f)  s
\end{code}


The following provide QuickCheck properties for the ModelState.
\begin{code}
-- Based on homework 

instance Arbitrary ModelState where
  arbitrary = sized modelStateGen

modelStateGen :: Int -> Gen ModelState
modelStateGen n = do
  model@(KrM u _ _) <- modelGen n
  state <- sublistOf u  -- choose a set from universe as state
  return (MS model state)

modelGen :: Int -> Gen KripkeModel
modelGen 0 = do
  let u = [0]  -- at least one world
  v <- arbitraryValuation u
  r <- arbitraryRelation u
  return $ KrM u v r
modelGen n = do
  size <- choose (1, n)
  let u = [0 .. fromIntegral size - 1]
  v <- arbitraryValuation u
  r <- arbitraryRelation u
  return $ KrM u v r

arbitraryValuation :: Universe -> Gen Valuation
arbitraryValuation u = do
  props <- vectorOf (length u) (listOf arbitrary)
  let val w = props !! fromIntegral w -- function
  return val

arbitraryRelation :: Universe -> Gen Relation
arbitraryRelation u = do
  pairs <- sublistOf [(x, y) | x <- u, y <- u]
  return (nub pairs)



\end{code}


A model state pair $(M,s)$ is indisputable if for all  $w,v \in s, R[w] = R[v]$. 


\begin{code}
isIndisputable :: ModelState -> Bool
isIndisputable (MS (KrM _ _ r) s) = any (\w -> any (\v -> sort (r ! w) == sort (r ! v )) s ) s
\end{code}


A model state pair is state-based if for all $w \in s, R[w] = s$.


\begin{code}
isStateBased :: ModelState -> Bool
isStateBased (MS (KrM _ _ r)  s) = any (\w -> sort (r ! w) == sort s) s
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
example11 = MS example1 [0,1,2]

example12 :: ModelState
example12 = MS example2 [0,1]


\end{code}

The following is a QuickCheck example, change this to a better tautology test.

\begin{code}
badTautology :: BSMLForm
badTautology =  Neg(Bot `Con` NE)

prop_tautologyHolds :: ModelState -> Bool
prop_tautologyHolds m = m |= badTautology

\end{code}