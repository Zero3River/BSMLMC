\section{The Syntax of BSML}\label{sec:Syntax}

Here, we describe the syntax of BSML with global disjunction:

\begin{code}
module Syntax where


type Prop = Int

data BSMLForm = P Prop | Bot | Neg BSMLForm | Con BSMLForm BSMLForm | Dis BSMLForm BSMLForm | Dia BSMLForm | NE | Gdis BSMLForm BSMLForm
  deriving (Eq,Ord,Show)

box :: BSMLForm -> BSMLForm
box = Neg . Dia . Neg
\end{code}

Note that Dis is the "$\lor$" disjunction, while Gdis is the "$\inqdisj$" disjunction.

The pragmatic enrichment function $[\,]^+ : \mathbf{ML} \to \mathbf{BSML}$ is describe recursively as follows:


\begin{code}
prag :: BSMLForm -> BSMLForm
prag (P n)      = Con (P n) NE
prag (Neg f)    = Con (Neg $ prag f) NE
prag (Con f g)  = Con (Con (prag f) (prag g)) NE
prag (Dis f g)  = Con (Dis (prag f) (prag g)) NE
prag (Dia f)    = Con (prag (Dia f)) NE
prag (Gdis f g) = Con (Gdis (prag f) (prag g)) NE
prag Bot        = Con Bot NE
prag NE         = undefined

\end{code}