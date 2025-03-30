\subsection{Testing Properties}
\label{sec:simpletests}
\hide{
\begin{code}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Syntax
import Semantics
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Data.Either
import Parser
\end{code}
}
We use the HSpec library to define different tests to check the correctness of our implementation.
We first begin with static tests based on examples defined earlier:

\begin{code}
main :: IO ()
main = hspec $ do
    describe "Static tests" $ do
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
\end{code}
The following checks that certain semantic properties hold, in particular the validity of FC inferences. For details, please refer to \cite{Aloni2022}:
\begin{code}      
    describe "BSML semantic Properties" $ modifyMaxSize (const 5)$ modifyMaxDiscardRatio(const 50)$ do
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
\end{code}
The final test checks that the parser works as intended
\begin{code}
    describe "Parser check" $ do
      it "parse . prettyPrint f == f" $
        property $ \f -> fromRight Bot (parseForm (ppBSML f)) == f
    where
        p = P 0
        q = P 1
        a = P 2
        b = P 3


\end{code}

To run the tests, use \verb|stack test|.

To also find out which part of your program is actually used for these tests,
run \\
\verb|stack clean && stack test --coverage|. Then look for ``The coverage
report for ... is available at ... .html'' and open this file in your browser.
See also: \url{https://wiki.haskell.org/Haskell_program_coverage}.
