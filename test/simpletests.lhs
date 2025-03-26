
\section{Simple Tests}
\label{sec:simpletests}

We now use the library QuickCheck to randomly generate input for our functions
and test some properties.

\begin{code}
module Main where

import Basics
import Syntax
import Semantics

import Test.Hspec
import Test.QuickCheck
\end{code}

The following uses the HSpec library to define different tests.
Note that the first test is a specific test with fixed inputs.
The second and third test use QuickCheck.

\begin{code}
main :: IO ()
main = hspec $ do
    describe "BSML Properties" $ do
      it "Narrow Scope FC" $
        property $ \ms@(MS m s) -> ms |= prag (Dia (Dis p q)) ==> ms |= Con (Dia p) (Dia q)
    
      it "Wide Scope FC" $
        property $ \ms@(MS m s) -> isIndisputable ms ==> ms|= prag (Dis (Dia p) (Dia q)) ==> ms |= Con (Dia p) (Dia q)
    
      it "Dual Prohibition" $
        property $ \ms@(MS m s) -> ms |= prag (Neg (Dis p q)) ==> ms |= Con (Neg $ Dia p) (Neg $ Dia q)
    
      it "Double Negation" $
        property $ \ms@(MS m s) -> ms |= prag (Neg . Neg . Dia $ Con p q) ==> ms |= Con (Dia p) (Dia q)
    
      it "Modal Disjunction" $
        property $ \ms@(MS m s) -> isStateBased ms ==> ms |= prag (Dis p q) ==> ms |= Con (Dia p) (Dia q)
    where
      p = P 1
      q = P 2

-- Helper function to define entailment between formulas
entails :: BSMLForm -> BSMLForm -> Bool
entails f g = True -- Placeholder, requires semantic evaluation

\end{code}

To run the tests, use \verb|stack test|.

To also find out which part of your program is actually used for these tests,
run \verb|stack clean && stack test --coverage|. Then look for ``The coverage
report for ... is available at ... .html'' and open this file in your browser.
See also: \url{https://wiki.haskell.org/Haskell_program_coverage}.
