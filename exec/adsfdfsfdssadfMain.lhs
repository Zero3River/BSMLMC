\section{Wrapping it up in an exectuable}\label{sec:Main} 

We will now use the library form Section \ref{sec:Basics} in a program.

\begin{code}
module Main where

-- import ModelChecker
import Syntax
import Semantics

testModel :: KripkeModel
testModel = KrM 
    [1, 2, 3]  
    (\w -> if w == 1 then [1] else if w == 2 then [2] else [3])  
    [(1, 2), (2, 3)]  

testFormula1 :: BSMLForm
testFormula1 = P 1 

testFormula2 :: BSMLForm
testFormula2 = Dia (P 3)  

testFormula3 :: BSMLForm
testFormula3 = Neg (P 1) 

testFormula4 :: BSMLForm
testFormula4 = Con (P 1) NE  

-- main :: IO ()
-- main = do
--     putStrLn "=== Running BSML Model Checker Tests ==="
--     putStrLn "Kripke Model:"
--     print testModel
--     putStrLn "\nChecking formulas:"
--     print $ ("P 1:", modelCheck testModel [1] testFormula1)  
--     print $ ("$\neg$ P 1:", modelCheck testModel [1] testFormula3) 
--     print $ ("$\\Diamond$ P 3:", modelCheck testModel [1] testFormula2) 
--     print $ ("P 1 $\\land$ NE:", modelCheck testModel [1] testFormula4)  
--     putStrLn "=== Tests Completed ==="

\end{code}

We can run this program with the commands:

\begin{verbatim}
stack build
stack exec bsml_checker
\end{verbatim}

The output of the program is something like this:

\begin{verbatim}

=== Running BSML Model Checker Tests ===
Kripke Model:
KrM [1,2,3] <function> [(1,2),(2,3)]

Checking formulas:

("P 1:", True)
("$\neg$ P 1:", False)
("$\\Diamond$P 3:", True)
("P 1 $\land$ NE:", True)

=== Tests Completed ===

\end{verbatim}
