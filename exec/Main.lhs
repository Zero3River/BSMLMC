\begin{code}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Text.Lazy (Text)
import qualified Data.String as DS (fromString)
import Data.Aeson (FromJSON, ToJSON, Object, encode, decode)
import Data.Aeson (object, (.=))
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Syntax
import Checker
import Data.List
import Data.Maybe
import qualified Data.Aeson.Key as Key

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

-- 将Input转换为ModelState
inputToModelState :: Input -> ModelState
inputToModelState input = (kripkeModel, state')
  where
    kripkeModel = KrM universe' valuation' relation'
    universe' = universe input
    
    -- 转换valuation成为一个函数
    valuation' :: Valuation
    valuation' world = case lookup world (valuation input) of
                         Just props -> props
                         Nothing -> []
    
    -- 将关系列表直接转换
    relation' = relation input
    
    -- 从输入中获取状态
    state' = state input

main :: IO ()
main = scotty 3000 $ do
    post "/input" $ do  -- 使用OverloadedStrings，不需要显式转换
        input <- jsonData :: ActionM Input
        let modelState = inputToModelState input
            (kripkeModel, state') = modelState
            KrM universe' valuation' relation' = kripkeModel
        json $ object [
            "result" .= isSupport input
          , "formula" .= formula input
          , "state" .= state'
          -- 可以添加更多信息...
          ]
\end{code}