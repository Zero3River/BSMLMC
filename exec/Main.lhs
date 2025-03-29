\subsection{Web server configuration}\label{sec:Main} 


The server is built using Scotty \cite{scotty} and listens for POST requests at /input:



\hide{
\begin{code}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Cors
-- import Data.Text.Lazy (Text)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
-- import Data.Aeson ()
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Syntax
import Parser
import Semantics
-- import Data.List
-- import Data.Maybe
-- import qualified Data.Aeson.Key as Key
-- import Text.Parsec
-- import Text.Parsec.String
-- import Text.Parsec.Expr
-- import Text.Parsec.Token
-- import Text.Parsec.Language
import Test.QuickCheck


data Person = Person
    { name :: String
    , age :: Int
    } deriving (Show, Generic, FromJSON, ToJSON)

data Input = Input
    { universe :: [Integer]
    , valuation :: [(Integer, [Int])]
    , relation :: [(Integer, Integer)]
    , state :: [Integer]
    , formula :: String
    , isSupport :: Bool
    , isPrag :: Bool
    } deriving (Show, Generic, FromJSON, ToJSON)

data InputQuickCheck = InputQuickCheck
    {
      formulaL :: String
      , formulaR :: String
      , isPragL :: Bool
      , isPragR :: Bool
    }  deriving (Show, Generic, FromJSON, ToJSON)


-- Turne Input into ModelState
inputToModelState :: Input -> ModelState
inputToModelState input = MS kripkeModel state'
  where
    kripkeModel = KrM universe' valuation' relation'
    universe' = universe input
    
    -- turn valuation into function
    valuation' :: Valuation
    valuation' world = case lookup world (valuation input) of
                         Just props -> props
                         Nothing -> []
    
    -- unmarshal relation
    relation' = relation input
    
    -- unmarshal state
    state' = state input

-- Check if formula is supported
-- checkFormula :: ModelState -> String -> Bool -> Bool
-- checkFormula modelState formulaStr isSupport = 
--   case parseForm formulaStr of
--     Left err -> error $ "Error in parsing formula: " ++ show err
--     Right parsedFormula -> 
--       if isSupport
--         then modelState |= parsedFormula
--         else modelState =| parsedFormula 

allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        }


prop_implicationHolds :: BSMLForm -> BSMLForm -> ModelState -> Bool
prop_implicationHolds f1 f2 m =
  not (m |= f1) || (m |= f2)

\end{code}
}




\begin{code}
main :: IO ()
main = scotty 3001 $ do
    middleware allowCors

    post "/input" $ do
        input <- jsonData :: ActionM Input
        let modelState = inputToModelState input
            (MS kripkeModel state') = modelState
            KrM _ _ relation' = kripkeModel
            
            -- Parse and Check Formula
            result = do
              parsedFormula <- parseForm (formula input)
              let finalFormula = if isPrag input then prag parsedFormula else parsedFormula
              return $ if isSupport input
                         then modelState |= finalFormula
                         else modelState =| finalFormula
                         
            -- Generate response
            finalResult = case result of
              Left err -> object [
                  "error" .= show err
                , "formula" .= formula input
                , "state" .= state'
                ]
              Right checkResult -> object [
                  "result" .= checkResult
                , "formula" .= formula input
                , "state" .= state'
                , "relation" .= show relation'
                , "relation_type" .= (if isSupport input then "support |=" else "reject =|" :: String)
                ]
            
        json finalResult
    post "/quickcheck" $ do
        inputQuickCheck <- jsonData :: ActionM InputQuickCheck

        -- Parse both formulas
        let pf1 = parseForm (formulaL inputQuickCheck)
            pf2 = parseForm (formulaR inputQuickCheck)

        case (pf1, pf2) of
          (Right f1, Right f2) -> do
            let finalF1 = if isPragL inputQuickCheck then prag f1 else f1
                finalF2 = if isPragR inputQuickCheck then prag f2 else f2

                prop m = prop_implicationHolds finalF1 finalF2 m

            result <- liftIO $ quickCheckWithResult stdArgs { maxSuccess = 30 } prop

            let jsonResult = case result of
                  Success {} -> object [
                      "status" .= ("passed" :: String),
                      "numTests" .= numTests result
                    ]
                  GaveUp {} -> object [
                      "status" .= ("gave up" :: String),
                      "reason" .= reason result
                    ]
                  Failure { output = out, usedSeed = _ } -> object [
                      "status" .= ("failed" :: String),
                      "reason" .= reason result,
                      "output" .= out
                    ]
                  NoExpectedFailure {} -> object [
                      "status" .= ("unexpected success" :: String)
                    ]

            json jsonResult

          _ -> json $ object [
              "status" .= ("parse error" :: String),
              "errorL" .= show pf1,
              "errorR" .= show pf2
            ]
          

\end{code}


\subsection{Usage}
Our BSML model checker web application is available at \url{https://bsmlmc.seit.me}.

Here we give an example to illustrate how to use the web application.

We define a model with universe:\{W1,W2,W3,W4\}, state:\{W2,W3\}.In W1, p1 and p2 are true; in W2, p1 is true; in W3, p2 is true; W4 is empty. 
W2 and W3 are reflexive and mutually related. We test whether formula $\Diamond p1 \land \Diamond p2$ holds in this model.
Figure \ref{fig:interface1} shows how to input worlds, valuations and relations in interface the. 
Figure \ref{fig:interface2} shows how to choose worlds in the state and input the formula. 
By clicking \textbf{Evaluate} bottow, web can return model-checking results.
Figure \ref{fig:model graph} displays a graphical representation of the model and state.


\begin{figure}[h]
    \centering
    \includegraphics[width=\textwidth]{image/p1.png}
    \caption{interface1}
    \label{fig:interface1}
\end{figure}

\begin{figure}[h]
    \centering
    \includegraphics[width=\textwidth]{image/p2.png}
    \caption{interface2}
    \label{fig:interface2}
\end{figure}

\begin{figure}[h]
    \centering
    \includegraphics[width=\textwidth]{image/p3.png}
    \caption{model graph}
    \label{fig:model graph}
\end{figure}
