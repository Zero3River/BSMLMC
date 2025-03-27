
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
import Test.Hspec.QuickCheck

\end{code}

The following uses the HSpec library to define different tests.
Note that the first test is a specific test with fixed inputs.
The second and third test use QuickCheck.

\begin{code}

main :: IO ()
main = hspec $ do
    describe "BSML Properties" $ modifyMaxSize (const 10)$ do
      it "Figure 2 state (a) |= a V b" $
         ms2a18 |= Dis a b `shouldBe` True

      it "Narrow Scope FC" $
        property $ \ms -> ms |= prag (Dia (Dis p q)) == ms |= Con (Dia p) (Dia q)
    
      it "Wide Scope FC" $
        property $ \ms -> isIndisputable ms ==> ms|= prag (Dis (Dia p) (Dia q)) ==> ms |= Con (Dia p) (Dia q)
    
      it "Dual Prohibition" $
        property $ \ms -> ms |= prag (Neg (Dis p q)) ==> ms |= Con (Neg $ Dia p) (Neg $ Dia q)
    
      it "Double Negation" $
        property $ \ms -> ms |= prag (Neg . Neg . Dia $ Con p q) ==> ms |= Con (Dia p) (Dia q)
    
      it "Modal Disjunction" $
        property $ \ms -> isStateBased ms ==> ms |= prag (Dis p q) ==> ms |= Con (Dia p) (Dia q)
    where
        p = P 0
        q = P 1
        a = P 1
        b = P 2


\end{code}

To run the tests, use \verb|stack test|.

To also find out which part of your program is actually used for these tests,
run \verb|stack clean && stack test --coverage|. Then look for ``The coverage
report for ... is available at ... .html'' and open this file in your browser.
See also: \url{https://wiki.haskell.org/Haskell_program_coverage}.
