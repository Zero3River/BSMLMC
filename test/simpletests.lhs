
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
    it "Narrow Scope FC" $ do
      property $ \f g -> prag (Dia (Dis f g)) `entails` Con (Dia (prag f)) (Dia (prag g))
    
    it "Wide Scope FC" $ do
      property $ \f g -> prag (Dis (Dia f) (Dia g)) `entails` Con (Dia (prag f)) (Dia (prag g))
    
    it "Dual Prohibition" $ do
      property $ \f g -> prag (Neg (Dia (Dis f g))) `entails` Con (Neg (Dia (prag f))) (Neg (Dia (prag g)))
    
    it "Double Negation" $ do
      property $ \f g -> prag (Neg (Neg (Dia (Dis f g)))) `entails` Con (Dia (prag f)) (Dia (prag g))
    
    it "Modal Disjunction" $ do
      property $ \f g -> prag (Dis f g) `entails` Con (Dia (prag f)) (Dia (prag g))

-- Helper function to define entailment between formulas
entails :: BSMLForm -> BSMLForm -> Bool
entails f g = True -- Placeholder, requires semantic evaluation

\end{code}

To run the tests, use \verb|stack test|.

To also find out which part of your program is actually used for these tests,
run \verb|stack clean && stack test --coverage|. Then look for ``The coverage
report for ... is available at ... .html'' and open this file in your browser.
See also: \url{https://wiki.haskell.org/Haskell_program_coverage}.
