\begin{code}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Web.Scotty
import Data.Text.Lazy (Text)
import Data.String (fromString)
import Data.Aeson (FromJSON, ToJSON, Object, encode, decode)
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)

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
    } deriving (Show, Generic, FromJSON, ToJSON)

main :: IO ()
main = scotty 3000 $ do
    get (fromString "/message") $ do
        text (fromString "Hello from Scotty!")
    
    post (fromString "/echo") $ do
        message <- body
        text $ fromString $ show message

    post (fromString "/person") $ do
        person <- jsonData :: ActionM Person
        json person  -- 自动将Person转换为JSON并返回

    post (fromString "/input") $ do
        input <- jsonData :: ActionM Input
        
        json input
\end{code}