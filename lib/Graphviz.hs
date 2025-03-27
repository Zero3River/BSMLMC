module Graphviz where

-- import Data.List (intercalate)
import Text.Printf (printf)
import Data.List
-- Types

type World = Integer
type Universe = [World]
type Proposition = Int
type State = [World]

type Valuation = World -> [Proposition]
type Relation = [(World,World)]

data KripkeModel = KrM Universe Valuation Relation
type ModelState = (KripkeModel, State)

-- Generate Graphviz DOT string, Based on the Graphviz documentation

generateDot :: ModelState -> String
generateDot (KrM universe val rel, state) =
  "digraph KripkeModel {\n" ++
  "  node [shape=circle, style=filled, fillcolor=white];\n" ++
  "  nodesep=1.0;\n" ++  
  "  ranksep=1.0;\n" ++  
  drawLightBlueSubgraph (filter (`elem` state) universe) ++  
  concatMap drawWorld (universe \\ state) ++ 
  concatMap drawRelation rel ++
  "}\n"
  where
    drawLightBlueSubgraph :: [Integer] -> String
    drawLightBlueSubgraph lightBlueNodes =
      if null lightBlueNodes then "" 
      else
        "  subgraph cluster_lightblue {\n" ++
        "    node [fillcolor=lightblue];\n" ++
        "    rank=same;\n" ++
        concatMap drawWorld lightBlueNodes ++
        "  }\n"

    drawWorld w =
        let color :: String
            color = if w `elem` state then "lightblue" else "white"
            label :: String
            label = printf "W%d\n%s" w (show (val w))
        in printf "  %d [label=\"%s\", fillcolor=%s];\n" w label color

    drawRelation (w1, w2) = printf "  %d -> %d;\n" w1 w2


-- Example Usage
exampleModel :: KripkeModel
exampleModel = KrM [0, 1, 2]
                    (\w -> if w == 0 then [1] else if w == 1 then [2] else [])
                    [(0, 1), (1, 2), (2, 0)]

exampleState :: State
exampleState = [0, 2]

exampleModelState :: ModelState
exampleModelState = (exampleModel, exampleState)

example1 :: KripkeModel
example1 = KrM [0,1,2] myVal [(0,1), (1,2), (2,1)] where
  myVal 0 = [0]
  myVal _ = [4]

example2 :: KripkeModel
example2 = KrM [0,1] myVal [(0,1), (1,1)] where
  myVal 0 = [0]
  myVal _ = [0, 4]

example11 :: ModelState
example11 = (example1, [0,1,2])

example12 :: ModelState
example12 = (example2, [0,1])

example3 :: KripkeModel
example3 = KrM [0,1,2,3,4,5,6,7] myVal [(0,1), (1,2), (2,1),(1,5),(2,4),(4,3),(5,6),(2,6),(6,4),(1,6)] where
  myVal 0 = [0]
  myVal 1 = [1]
  myVal 2 = [2,8]
  myVal 3 = [3]
  myVal 4 = [4,1,5]
  myVal 5 = [5]
  myVal 6 = [1,2,3,4,5]
  myVal 7 = [7]
  myVal _ = []

example31 :: ModelState
example31 = (example3, [1,3,4,6,7])
main :: IO ()
main = putStrLn $ generateDot example31
