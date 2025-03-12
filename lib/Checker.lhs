

\section{The Definiton of Model Checker Data Type}\label{sec:Basics}

The following is the definition of our Data Type for Model Checker.

\begin{code}
{-# LANGUAGE InstanceSigs #-}
module Checker where



import Control.Monad
import System.Random
import Test.QuickCheck
import Data.List
-- * Question 5: Modal Logic

type World = Integer
type Universe = [World]
type Proposition = Int

type Valuation = World -> [Proposition]
type Relation = [(World,World)]

data KripkeModel = KrM Universe Valuation Relation

instance Show KripkeModel where
  show (KrM u v r) = "KrM " ++ show u ++ " " ++ vstr ++ " " ++ show r where
    vstr = "(fromJust . flip lookup " ++ show [(w, v w) | w <- u] ++ ")"

example1 :: KripkeModel
example1 = KrM [0,1,2] myVal [(0,1), (1,2), (2,1)] where
  myVal 0 = [0]
  myVal _ = [4]

example2 :: KripkeModel
example2 = KrM [0,1] myVal [(0,1), (1,1)] where
  myVal 0 = [0]
  myVal _ = [4]

data ModForm = P Proposition
             | Not ModForm
             | Con ModForm ModForm
             | Box ModForm
             | Dia ModForm
             deriving (Eq,Ord,Show)

(!) :: Relation -> World -> [World]
(!) r w = map snd $ filter ((==) w . fst) r

makesTrue :: (KripkeModel,World) -> ModForm -> Bool
makesTrue (KrM _ v _, w) (P k)     = k `elem` v w
makesTrue (m,w)          (Not f)   = not (makesTrue (m,w) f)
makesTrue (m,w)          (Con f g) = makesTrue (m,w) f && makesTrue (m,w) g
makesTrue (KrM u v r, w) (Box f)   = all (\w' -> makesTrue (KrM u v r,w') f) (r ! w)
makesTrue (KrM u v r, w) (Dia f)   = any (\w' -> makesTrue (KrM u v r,w') f) (r ! w)
tex :: ModForm -> String
tex = go
  where
    go  (P k)     = "p_{" ++ show k ++ "}"
    go  (Not f)   = "\\lnot " ++ go  f
    go  (Con f g) = "(" ++ go  f ++ " \\land " ++ go  g ++ ")"
    go  (Box f)   = "\\Box " ++ go  f
    go  (Dia f)   = "\\Diamond " ++ go  f

trueEverywhere :: KripkeModel -> ModForm -> Bool
trueEverywhere m@(KrM u _ _) f = all (\w -> makesTrue (m, w) f) u



instance Eq KripkeModel where
  (==) :: KripkeModel -> KripkeModel -> Bool
  (KrM u1 v1 r1) == (KrM u2 v2 r2) =
    sameSet u1 u2 &&
    sameSet (sort r1) (sort r2) &&
    all (\w -> sort (v1 w) == sort (v2 w)) u1

sameSet :: (Ord a, Eq a) => [a] -> [a] -> Bool
sameSet xs ys = sort (nub xs) == sort (nub ys)


type Bisimulation = [(World,World)]

exampleBisim :: Bisimulation
exampleBisim = [(0,0), (1,1), (2,1)]

checkBisim :: KripkeModel -> KripkeModel -> Bisimulation -> Bool

checkBisim (KrM _ v1 r1) (KrM _ v2 r2) bisim =
  all checkAtomic bisim && all checkZig bisim && all checkZag bisim
  where
    checkAtomic (w1, w2) = sort (v1 w1) == sort (v2 w2)

    checkZig (w1, w2) =
      all (\w1' -> any (\w2' -> (w1', w2') `elem` bisim) (r2 ! w2)) (r1 ! w1)

    checkZag (w1, w2) =
      all (\w2' -> any (\w1' -> (w1', w2') `elem` bisim) (r1 ! w1)) (r2 ! w2)



type EquiRel = [[World]]

data KripkeModelS5 = KrMS5 Universe Valuation EquiRel

instance Show KripkeModelS5 where
  show (KrMS5 u v r) = "KrMS5 " ++ show u ++ " " ++ vstr ++ " " ++ show r where
    vstr = "(fromJust . flip lookup " ++ show [(w, v w) | w <- u] ++ ")"

example3 :: KripkeModelS5
example3 = KrMS5 [0,1,2] myVal [[0],[1,2]] where
  myVal 0 = [3]
  myVal 1 = [2]
  myVal _ = []


findClass :: World -> EquiRel -> [World]
findClass w rel = head [cls | cls <- rel, w `elem` cls]

makesTrueS5 :: (KripkeModelS5, World) -> ModForm -> Bool
makesTrueS5 (KrMS5 _ v _, w) (P p) = p `elem` v w
makesTrueS5 (m, w) (Not f) = not (makesTrueS5 (m, w) f)
makesTrueS5 (m, w) (Con f g) = makesTrueS5 (m, w) f && makesTrueS5 (m, w) g
makesTrueS5 (KrMS5 u v rel, w) (Box f) =
  all (\w' -> makesTrueS5 (KrMS5 u v rel, w') f) (findClass w rel)
makesTrueS5 (KrMS5 u v rel, w) (Dia f) =
  any (\w' -> makesTrueS5 (KrMS5 u v rel, w') f) (findClass w rel)


class Semantics a where
  (|=) :: (a, World) -> ModForm -> Bool

instance Semantics KripkeModel where
  (|=) = makesTrue

instance Semantics KripkeModelS5 where
  (|=) = makesTrueS5

instance Arbitrary ModForm where
  arbitrary = sized randomModForm where
    randomModForm :: Int -> Gen ModForm
    randomModForm 0 = P <$> elements [1..5]
    randomModForm n = oneof [ P <$> elements [1..5]
                         , Not <$> randomModForm (n `div` 2)
                         , Con <$> randomModForm (n `div` 2)
                                <*> randomModForm (n `div` 2)
                         , Box <$> randomModForm (n `div` 2)
                         , Dia <$> randomModForm (n `div` 2) ]

-- Please ensure that all generated models have a world 0.

instance Arbitrary KripkeModel where
  arbitrary = sized modelGen

modelGen :: Int -> Gen KripkeModel
modelGen 0 = do
  let u = [0]  -- Universe with only world 0
  v <- arbitraryValuation u
  r <- arbitraryRelation u
  return $ KrM u v r
modelGen n = do
  size <- choose (1, n)  -- Universe size
  let u = [0..fromIntegral size - 1]  -- Ensure world 0 is included
  v <- arbitraryValuation u
  r <- arbitraryRelation u
  return $ KrM u v r
instance Arbitrary KripkeModelS5 where
  arbitrary = sized modelGenS5

modelGenS5 :: Int -> Gen KripkeModelS5
modelGenS5 0 = do
  let u = [0]
  v <- arbitraryValuation u
  let rel = [[0]]
  return $ KrMS5 u v rel
modelGenS5 n = do
  size <- choose (1, n)
  let u = [0..fromIntegral size - 1]
  v <- arbitraryValuation u
  rel <- arbitraryEquiRel u
  return $ KrMS5 u v rel

-- helpers
arbitraryValuation :: Universe -> Gen Valuation
arbitraryValuation u = do
  props <- vectorOf (length u) (listOf arbitrary)
  let val w = props !! fromIntegral w -- function
  return val

arbitraryRelation :: Universe -> Gen Relation
arbitraryRelation u = do
  pairs <- sublistOf [(x, y) | x <- u, y <- u]
  return (nub pairs)

arbitraryEquiRel :: Universe -> Gen EquiRel
arbitraryEquiRel = partitionUniverse

partitionUniverse :: Universe -> Gen EquiRel
partitionUniverse [] = return []
partitionUniverse u = do
  subset <- sublistOf u `suchThat` (not . null)
  rest <- partitionUniverse (u \\ subset) -- set subtraction
  return (subset : rest)

-- used for finding examples for (i)
generateModel :: IO KripkeModel
generateModel = generate arbitrary
findModel :: IO KripkeModel
findModel = do
  let mod_form = Con (P 1) (Not (Box (P 2)))
  model <- generateModel
  if trueEverywhere model mod_form
    then return model
    else findModel

generateModForm :: IO ModForm
generateModForm = generate arbitrary
findModForm :: IO ModForm
findModForm = do
  modForm <- generateModForm
  if trueEverywhere example1 modForm && not (trueEverywhere example2 modForm)
    then return modForm
    else findModForm

findModForm2 :: IO ModForm
findModForm2 = do
  modForm <- generateModForm
  if (example1,1) |= modForm && not ((example3,1) |= modForm)
    then return modForm
    else findModForm2

{-

Using QuickCheck to find examples:

(i) a model that satisfies p1 ^ ~[]p2:

helper functions:
generateModel :: IO KripkeModel
generateModel = generate arbitrary
findModel :: IO KripkeModel
findModel = do
  model@(KrM u _ _) <- generateModel
  if all(\x -> (model, x) |= Con (P 1) (Not (Box (P 2)))) u
    then return model
    else findModel

query: findModel
output: KrM [0] (fromJust . flip lookup [(0,[-1,-18,-7,-29,21,1,23,-20])]) [(0,0)]


(ii) a formula that is globally true in example1 but not globally true in example2:

helper functions:
generateModForm :: IO ModForm
generateModForm = generate arbitrary
findModForm :: IO ModForm
findModForm = do
  modForm <- generateModForm
  if trueEverywhere example1 modForm && not (trueEverywhere example2 modForm)
    then return modForm
    else findModForm

query: findModForm
output: No result, keeps on running forever. 
We can see that there is no such formula that is globally true in example1 but not globally true in example2.
But as suggested in the homework pdf, this is not a proof.

(iii) a formula that is true at 1 in example1 but not true at 1 in example3:

helper functions:
generateModForm :: IO ModForm
generateModForm = generate arbitrary
findModForm2 :: IO ModForm
findModForm2 = do
  modForm <- generateModForm
  if (example1,1) |= modForm && not (example3,1) |= modForm
    then return modForm
    else findModForm2
  
query: findModForm2
output: Dia (Dia (Box (P 4)))

-}


tests :: [Bool]
tests = [ makesTrueS5 (example3,0) (Con (Box (P 3)) (Box (Not (P 2))))
        , (example1,1) |= Not (Box (Dia (Not (P 4))))  
        , (example2,0) |= Dia (P 4)                    
        , (example2,1) |= Box (P 4)                    
        , (example3,0) |= Not (Dia (P 2))              
        , (example3,1) |= Not (Con (P 2) (Dia (P 3)))
        ]
\end{code}