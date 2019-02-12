module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data Direction = S | L | R | U | D
  deriving (Show)

type Vertex = Int
type Path = [(Maybe Direction, Vertex)]
type Edges = IntMap (Maybe Direction, Vertex)

data Graph = Graph {
    vertices :: [Vertex]
  , edges :: IntMap [(Int, Vertex, Maybe Direction)]
  , people :: IntMap Int
--  , names :: IntMap String
  , fire :: [Vertex]
  , goals :: [Vertex]
  } deriving (Show)

updatePeople :: Int -> Graph -> Vertex -> Graph
updatePeople n (Graph vs es ps f gs) v = 
    Graph vs es (IntMap.insert v ((ps IntMap.! v) + n) ps) f gs

leaveVertex :: Graph -> Vertex -> Graph
leaveVertex = updatePeople (-1)

visitVertex :: Graph -> Vertex -> Graph
visitVertex = updatePeople 1

edgesFrom :: Graph -> Vertex -> [(Int, Vertex, Maybe Direction)]
edgesFrom g v = case IntMap.lookup v (edges g) of
    Just x -> x
    Nothing -> error "No edges from vertex!"

minPath :: [(Int, Path)] -> (Int, Path)
minPath xs = aux xs (100000, [])
    where aux ((w', p') : xs) (w, p) = aux xs (if w < w' then (w, p) else (w', p'))
          aux [] (w, p) = (w, p)

visited :: Path -> Vertex -> Bool
visited ((_, v) : p) v' = v == v' || visited p v'
visited [] v' = False

findPath' :: Graph -> Vertex -> Int -> Path -> (Int, Path)
findPath' g v w acc 
    | v `elem` goals g = (w, acc) 
    | v `elem` fire g = (100000, [])
    | visited acc v = (100000, [])
    | otherwise = 
        minPath [findPath' g v' (w + w') ((d', v) : acc) | (w', v', d') <- edgesFrom g v]
    
findPath :: Graph -> Vertex -> Path
findPath g v = (\(_, y) -> y) $ findPath' g v 0 []

step :: Graph -> Path -> (Vertex, Graph)
step g ((_, v) : (_, v') : p) = (v', visitVertex (leaveVertex g v) v')

graph :: Graph
graph = Graph [1, 2, 3] (IntMap.fromList [(1, [(1, 2, Just L), (5, 3, Just R)]), (2, [(3, 3, Just U)])]) [2] [3]



main :: IO ()
main = do p <- return $ findPath graph 1
          putStrLn (show p)
