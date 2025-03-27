\section{The Syntax of BSML}\label{sec:Syntax}

The syntax of $\textbf{BSML}$ is defined over a set of propositional variables \(\text{Prop}\). The formulas of BSML are generated by the following grammar:

\[
\varphi ::= p \mid \bot \mid \neg \varphi \mid (\varphi \land \varphi) \mid (\varphi \lor \varphi) \mid \Diamond \varphi \mid \text{NE}
\]

where \(p \in \text{Prop}\). 

$\textbf{BSML}^{\inqdisj}$ extends $\textbf{BSML}$ with global disjunction $\inqdisj$ by adding the clause $\varphi \inqdisj \varphi$. 

The classical modal logic \textbf{ML} is the NE-free fragment of BSML.

Below is the implementation of the syntax of $\BSML^{\inqdisj}$. For the sake of brevity, we use the type \texttt{BSMLForm} for the formulas of $\BSML^{\inqdisj}$. We use \texttt{Int} to index the set of propositional variables.

\begin{code}
module Syntax where


type Prop = Int

data BSMLForm = P Prop | Bot | Neg BSMLForm | Con BSMLForm BSMLForm | Dis BSMLForm BSMLForm | Dia BSMLForm | NE | Gdis BSMLForm BSMLForm
  deriving (Eq,Ord,Show)
\end{code}
Note that \texttt{Dis} is the "$\lor$" disjunction, while \texttt{Gdis} is the "$\inqdisj$" disjunction.

The box modality \(\Box\) is defined as the dual of the \(\Diamond\): \(\Box \varphi := \neg \Diamond \neg \varphi\).

\begin{code}
box :: BSMLForm -> BSMLForm
box = Neg . Dia . Neg
\end{code}


The pragmatic enrichment function [\quad]\(^+\): \textbf{ML} \(\rightarrow\) \BSML is recursively defined as:

\begin{align*}
{[p]}^+ &\coloneqq p \land \NE \\
{[\bigcirc a]}^+ &\coloneqq \bigcirc({[a]}^+)\land \NE  \quad \quad \text{for } \bigcirc \in \{\neg,\Diamond,\Box\}\\
{[\alpha \mathrel{\triangle} \beta ]}^+ &\coloneqq ({[\alpha]}^+ \triangle {[\beta]}^+)\land \NE \quad \quad \text{for } \triangle \in \{\land, \lor\}
\end{align*}

We implment pragmatic enrichment \texttt{prag} as a partial function \texttt{BSMLForm -> BSMLForm}, as the $\textbf{ML}$ formulas are not used anywhere else in this project. 
\begin{code}
prag :: BSMLForm -> BSMLForm
prag (P n)      = Con (P n) NE
prag (Neg f)    = Con (Neg $ prag f) NE
prag (Con f g)  = Con (Con (prag f) (prag g)) NE
prag (Dis f g)  = Con (Dis (prag f) (prag g)) NE
prag (Dia f)    = Con (Dia (prag f)) NE
prag (Gdis f g) = Con (Gdis (prag f) (prag g)) NE
prag Bot        = Con Bot NE
prag NE         = error "Cannot pragmatically enrich formulas containing NE"

\end{code}