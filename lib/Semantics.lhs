\subsection{Semantics}\label{sec:Semantics}


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


The following is the definition of our Data Type for Model Checker.


\begin{code}
-- Based on the Homework

-- {-# LANGUAGE InstanceSigs #-}
module Semantics where

import Test.QuickCheck
import Data.List

import Syntax
import Data.Maybe



type World = Integer
type Universe = [World]
type Proposition = Int
type State = [World]

type Valuation = World -> [Proposition]
type Relation = [(World,World)]

data KripkeModel = KrM Universe Valuation Relation

data ModelState = MS KripkeModel State

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
(MS (KrM _ v _) s) |= (P n) = all (\w -> n `elem` v w) s
(MS _ s) |= Bot = null s
(MS _ s) |= NE = not $ null s
(MS (KrM u v r) s) |= (Neg f) = MS (KrM u v r) s =| f
m |= (Con f g) = m |= f && m |= g
(MS k s) |= (Dis f g) = any (\(ts,us) -> MS k ts |= f && MS k us |= g) (allPairs s)
m |= (Gdis f g) = m |= f || m |= g
(MS (KrM u v r) s) |= (Dia f) = all (\w -> any (\l -> MS (KrM u v r) l |= f ) (subsetsNonEmpty (r ! w)))  s



(=|) :: ModelState -> BSMLForm -> Bool
(MS (KrM _ v _) s) =| (P n) = all (\w -> n `notElem` v w) s
(MS _ _) =| Bot = True
(MS _ s) =| NE = null s
(MS (KrM u v r) s) =| (Neg f) = MS (KrM u v r) s |= f
(MS k s) =| (Con f g) = any (\(ts,us) -> MS k ts =| f && MS k us =| g) (allPairs s)
m =| (Dis f g) = m =| f && m =| g
m =| (Gdis f g) = m =| f && m =| g
(MS (KrM u v r) s)  =| (Dia f) = all (\w -> MS (KrM u v r) (r ! w) =| f)  s
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
modelGen n = do
  size <- choose (1, n)
  let u = [0 .. fromIntegral size - 1]
  v <- arbitraryValuation u
  r <- arbitraryRelation u
  return $ KrM u v r

arbitraryValuation :: Universe -> Gen Valuation
arbitraryValuation u = do
  props <- vectorOf (length u) (sublistOf [0..10]) -- fixed vocabulary
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
isIndisputable (MS (KrM _ _ r) s) = all (\w -> all (\v -> sort (r ! w) == sort (r ! v )) s ) s
\end{code}


A model state pair is state-based if for all $w \in s, R[w] = s$.


\begin{code}
isStateBased :: ModelState -> Bool
isStateBased (MS (KrM _ _ r)  s) = all (\w -> sort (r ! w) == sort s) s
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

Here, we encode the example in Figure 2, \cite{Aloni2018}. Here, \texttt{a = P 2, b = P 3}:

\begin{code}
w0, wa, wb, wab :: Integer
(w0, wa, wb, wab) = (0,1,2,3)

val2a18 :: Valuation
val2a18 = fromJust . flip lookup [(w0,[]), (wa,[2]),(wb,[3]),(wab,[2,3])]

m2a18 :: KripkeModel
m2a18 = KrM [w0,wa,wb,wab] val2a18 []

ms2a18, ms2b18 :: ModelState
ms2a18 = MS m2a18 [wa, wb]
ms2b18 = MS m2a18 [wa] 
\end{code}

Here is another example, from Figure 2(c), \cite{Aloni2024}. Here \texttt{p = P 0, q = P 1}:

\begin{code}
wp, wq, wpq :: Integer
(wp, wq, wpq) = (1,2,3) -- same w0 as above

val2c24 :: Valuation
val2c24 = fromJust . flip lookup [(w0,[]),(wp,[0]),(wq,[1]),(wpq,[0,1])]

rel2c24 :: Relation
rel2c24 = [(w0,wq), (wpq,wp), (wpq,wq)]

m2c24 :: KripkeModel
m2c24 = KrM [w0,wp,wq,wpq] val2c24 rel2c24

ms2c241, ms2c242 :: ModelState
ms2c241 = MS m2c24 [w0]
ms2c242 = MS m2c24 [wpq] 
\end{code}

The following example is from Figure 3(a), \cite{Aloni2022}
\begin{code}
val3a22 :: Valuation
val3a22 = val2a18

rel3a22 :: Relation
rel3a22 = [(wa,w0), (wa,wab), (wb,w0), (wb,wab)]

m3a22 :: KripkeModel
m3a22 = KrM [w0,wa,wb,wab] val3a22 rel3a22

ms3a22 :: ModelState
ms3a22 = MS m3a22 [wa,wb]

counterexamplews1 :: ModelState
counterexamplews1 = MS (KrM [0,1,2,3,4] (fromJust . flip lookup [(0,[0,4,6,8,9,10]),(1,[1,4,6,8]),(2,[0,3,5,8,9]),(3,[0,1,4,6,8]),(4,[1,2,3,7,8])]) [(0,0),(1,1),(1,4),(2,1),(2,4),(3,1),(3,2),(4,1)] ) [0,2,3]

counterexamplewmd :: ModelState
counterexamplewmd = MS (KrM [0,1,2] (fromJust . flip lookup [(0,[1,3,4,7,9]),(1,[0,2,6,7,8,9,10]),(2,[1,6,8,9])]) [(1,0),(1,1),(2,1),(2,2)]) [0,1]

p :: BSMLForm
p = P 0
q :: BSMLForm
q = P 1
\end{code}

The following is a QuickCheck example, change this to a better tautology test.

\begin{code}
badTautology :: BSMLForm
badTautology =  Neg(Bot `Con` NE)

prop_tautologyHolds :: ModelState -> Bool
prop_tautologyHolds m = m |= badTautology

\end{code}