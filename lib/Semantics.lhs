\begin{code}
module Semantics where

import Syntax

prag :: BSMLForm -> BSMLForm
prag (P n)      = Con (P n) NE   
prag (Neg f)    = Con (Neg $ prag f) NE  
prag (Con f g)  = Con (Con (prag f) (prag g)) NE  
prag (Dis f g)  = Con (Dis (prag f) (prag g)) NE  
prag (Dia f)    = Con (prag (Dia f)) NE  
prag (Gdis f g) = Con (Gdis (prag f) (prag g)) NE  
prag Bot        = Con Bot NE 
prag NE         = undefined   


(|=) :: ModelState -> BSMLForm -> Bool
(KrM _ v _, s) |= (P p)     = all (\w -> p `elem` v w) s
(KrM _ _ _, s) |= Bot       = null s
(KrM _ _ _, s) |= NE        = not (null s)
(KrM m _ _, s) |= (Neg f)   = (m, s) =| f  
(KrM m _ _, s) |= (Con f g) = (m, s) |= f && (m, s) |= g
(KrM m _ _, s) |= (Dis f g) = any (\(t, u) -> (m, t) |= f && (m, u) |= g) (splitState s)
(KrM m r _, s) |= (Dia f)   = all (\w -> any (\t -> (m, t) |= f) (nonEmptySubsets (successors r w))) s
(KrM m _ _, s) |= (Gdis f g) = (m, s) |= f || (m, s) |= g


(=|) :: ModelState -> BSMLForm -> Bool
(KrM _ v _, s) =| (P p)     = any (\w -> p `notElem` v w) s
(KrM _ _ _, s) =| Bot       = False 
(KrM _ _ _, s) =| NE        = null s
(KrM m _ _, s) =| (Neg f)   = (m, s) |= f 
(KrM m _ _, s) =| (Con f g) = any (\(t, u) -> (m, t) =| f && (m, u) =| g) (splitState s)
(KrM m _ _, s) =| (Dis f g) = (m, s) =| f && (m, s) =| g
(KrM m r _, s) =| (Dia f)   = any (\w -> (m, successors r w) =| f) s
(KrM m _ _, s) =| (Gdis f g) = (m, s) =| f && (m, s) =| g


splitState :: State -> [(State, State)]
splitState s = [(t, s \\ t) | t <- subsequences s, not (null t)]


nonEmptySubsets :: [World] -> [State]
nonEmptySubsets w = filter (not . null) (subsequences w)

\end{code}