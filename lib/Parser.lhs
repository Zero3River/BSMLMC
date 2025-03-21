

\begin{code}
module Parser where

import Syntax
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Semantics

pForm :: Parsec String () BSMLForm
pForm = spaces >> pCnt <* (spaces >> eof) where
  pCnt =  chainl1 pDia (spaces >> (pGdis <|> pDisj <|> pConj))
  
  pConj = char '&' >> return Con
  pDisj = char '|' >> return Dis
  pGdis = char '/' >> return Gdis

  -- Diamond operator has higher precedence than conjunction
  pDia = try pDiaOp <|> pAtom
  pDiaOp = spaces >> char '<' >> char '>' >> Dia <$> pAtom
  
  -- An atom is a variable, negation, or a parenthesized formula
  pAtom = spaces >> (pBot <|> pNE <|> pVar <|> pNeg <|> (spaces >> char '(' *> pCnt <* char ')' <* spaces))
  
  -- A variable is 'p' followed by digits
  pVar = char 'p' >> P . read <$> many1 digit <* spaces
  
  pBot = string "bot" >> return Bot
  pNE = string "ne" >> return NE

  -- A negation is '!' followed by an atom
  pNeg = char '!' >> Neg <$> pAtom

parseForm :: String -> Either ParseError BSMLForm
parseForm = parse pForm "input"

parseForm' :: String -> BSMLForm
parseForm' s = case parseForm s of
  Left e -> error $ show e
  Right f -> f

\end{code}