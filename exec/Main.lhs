\section{Wrapping it up in an exectuable}\label{sec:Main} 


\begin{code}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Cors  -- 引入 CORS 中间件
import Data.Text.Lazy (Text)
import Data.Aeson (FromJSON, ToJSON, Object, encode, decode)
import Data.Aeson (object, (.=))
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Syntax
import Parser
import Semantics
import Data.List
import Data.Maybe
import qualified Data.Aeson.Key as Key
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

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
inputToModelState input = MS kripkeModel state'
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

-- 检查公式是否在给定的模型状态下成立
checkFormula :: ModelState -> String -> Bool -> Bool
checkFormula modelState formulaStr isSupport = 
  case parseForm formulaStr of
    Left err -> error $ "解析公式错误: " ++ show err
    Right parsedFormula -> 
      if isSupport
        then modelState |= parsedFormula  -- 支持关系 |=
        else modelState =| parsedFormula  -- 拒绝关系 =|

allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        }

main :: IO ()
main = scotty 3001 $ do
    middleware allowCors

    post "/input" $ do
        input <- jsonData :: ActionM Input
        let modelState = inputToModelState input
            (MS kripkeModel state') = modelState
            KrM universe' valuation' relation' = kripkeModel
            
            -- 解析并检查公式
            result = do
              parsedFormula <- parseForm (formula input)
              return $ if isSupport input
                         then modelState |= parsedFormula  -- 支持关系
                         else modelState =| parsedFormula  -- 拒绝关系
                         
            -- 根据解析结果生成响应
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
\end{code}