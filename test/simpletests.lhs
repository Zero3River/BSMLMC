
\section{Simple Tests}
\label{sec:simpletests}

We now use the library QuickCheck to randomly generate input for our functions
and test some properties.

\begin{code}
module Main where

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
      it "Figure 2, Aloni2018 [wa,wb] |= a | b" $
        ms2a18 |= Dis a b `shouldBe` True      
      it "Figure 2, Aloni2018 [wa,wb] not |= a / b" $
        ms2a18 |= Gdis a b `shouldBe` False
      it "Figure 2, Aloni2018 [wa] |= (a | b) & (a / b)" $
        ms2b18 |= Con (Dis a b) (Gdis a b) `shouldBe` True
      it "Figure 2(c), Aloni2024  [w0] not |= [<>(p | q)]+" $
        ms2c241 |= prag (Dia (Dis p q)) `shouldBe` False
      it "Figure 2(c), Aloni2024  [wpq] |= [<>(p | q)]+" $
        ms2c241 |= prag (Dia (Dis p q)) `shouldBe` False    
      it "Figure 2(a), Aloni2022  [wa,wb] is indisputable" $
        isIndisputable ms3a22 `shouldBe` True   
      it "Figure 2(a), Aloni2022  [wa,wb] is not state-based" $
        isStateBased ms3a22 `shouldBe` False      
      it "State-basedness implies indisputability" $
        property $ \ms -> isStateBased ms ==> isIndisputable ms
      it "Narrow Scope FC" $
        property $ \ms -> ms |= prag (Dia (Dis p q)) ==> ms |= Con (Dia p) (Dia q)    
      it "Wide Scope FC" $
        property $ \ms -> isIndisputable ms ==> ms |= prag (Dis (Dia p) (Dia q)) ==> ms |= Con (Dia p) (Dia q)    
      it "Dual Prohibition" $
        property $ \ms -> ms |= prag ((Neg . Dia) (Dis p q)) ==> ms |= Con (Neg (Dia p)) (Neg (Dia q))    
      it "Double Negation" $
        property $ \ms -> ms |= prag ((Neg . Neg . Dia) (Dis p q)) ==> ms |= Con (Dia p) (Dia q)    
      it "Modal Disjunction" $
        property $ \ms -> isStateBased ms ==> ms |= prag (Dis p q) ==> ms |= Con (Dia p) (Dia q)
    where
        p = P 0
        q = P 1
        a = P 2
        b = P 3


\end{code}

To run the tests, use \verb|stack test|.

To also find out which part of your program is actually used for these tests,
run \verb|stack clean && stack test --coverage|. Then look for ``The coverage
report for ... is available at ... .html'' and open this file in your browser.
See also: \url{https://wiki.haskell.org/Haskell_program_coverage}.
