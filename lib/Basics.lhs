
\section{The most basic library}\label{sec:Basics}

Here we describe the syntax of BSML

\begin{code}
module Basics where

import Control.Monad
import System.Random

type Prop = Int

data BSMLForm = P Prop | Neg BSMLForm | Con BSMLForm BSMLForm | Dis BSMLForm BSMLForm | Dia BSMLForm | NE | Gdis BSMLForm BSMLForm
  deriving (Eq,Ord,Show)

box :: BSMLForm -> BSMLForm
box = Neg . Dia . Neg

\end{code}

Note that Dis is the "V" disjunction, while Gdis is the "\V" disjunction.

\begin{code}
thenumbers :: [Integer]
thenumbers = [1..]

somenumbers :: [Integer]
somenumbers = take 10 thenumbers

randomnumbers :: IO [Integer]
randomnumbers = replicateM 10 $ randomRIO (0,10)
\end{code}

We can interrupt the code anywhere we want.

\begin{code}
funnyfunction :: Integer -> Integer
funnyfunction 0 = 42
\end{code}

Even in between cases, like here.
It's always good to cite something \cite{Knuth11CombAlg}.

\begin{code}
funnyfunction n | even n    = funnyfunction (n-1)
                | otherwise = n*100
\end{code}

Something to reverse lists.

\begin{code}
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]
\end{code}

If you look at the \texttt{.lhs} file then below this line you can find some Haskell code.

\hide{
\begin{code}
secret :: Int
secret = 4
\end{code}
}

But it does not show up in the PDF document.
Please only use this for boring or repetitive parts of your code.
Do not hide too much from your reader.

That's it, for now.
