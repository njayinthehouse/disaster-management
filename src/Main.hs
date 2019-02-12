module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Concurrent

capacity :: Int
capacity = 1

players :: Int
players = 5

type Vertex = Int
type Path = [Vertex]

data Graph = Graph {
    vertices :: [Vertex]
  , edges :: IntMap [(Int, Vertex)]
  , people :: IntMap Int
--  , names :: IntMap String
  , fire :: [Vertex]
  , goals :: [Vertex]
  } deriving (Show)

lookupOrRet :: IntMap a -> Int -> a -> a
lookupOrRet m k n = case IntMap.lookup k m of
    Just x -> x
    Nothing -> n

updatePeople :: Int -> Graph -> Vertex -> Graph
updatePeople n (Graph vs es ps f gs) v = 
    Graph vs es (IntMap.insert v ((lookupOrRet ps v 0) + n) ps) f gs

addEdge :: Graph -> (Vertex, (Int, Vertex)) -> Graph
addEdge (Graph vs es ps f gs) (v, (i, u)) = 
    Graph vs (IntMap.insert v ((i, u) : (lookupOrRet es v [])) es) ps f gs

addEdges :: Graph -> [(Vertex, (Int, Vertex))] -> Graph
addEdges g (e : es) = addEdges (addEdge g e) es
addEdges g [] = g

updateWeights :: Graph -> Vertex -> Int -> Graph
updateWeights (Graph vs es ps f gs) v c = 
    Graph vs 
          (IntMap.map (\l -> map (\(w', v') -> (if (lookupOrRet ps v' 0) >= c then 1000 + w' else w', v')) l) es)
          ps f gs

leaveVertex :: Graph -> Vertex -> Graph
leaveVertex = updatePeople (-1)

visitVertex :: Graph -> Vertex -> Graph
visitVertex = updatePeople 1

edgesFrom :: Graph -> Vertex -> [(Int, Vertex)]
edgesFrom g v = case IntMap.lookup v (edges g) of
    Just x -> x
    Nothing -> error (show v)

minPath :: [(Int, Path)] -> (Int, Path)
minPath xs = aux xs (100000, [])
    where aux ((w', p') : xs) (w, p) = aux xs (if w < w' then (w, p) else (w', p'))
          aux [] (w, p) = (w, p)

visited :: Path -> Vertex -> Bool
visited (v : p) v' = v == v' || visited p v'
visited [] v' = False

findPath' :: Graph -> Vertex -> Int -> Int -> Path -> (Int, Path)
findPath' g v c w acc 
    | v `elem` goals g = (w, acc) 
    | v `elem` fire g = (1000000 + w, [])
    | visited acc v = (100000 + w, [])
    | otherwise = 
        minPath [findPath' g v' c (w + w') (v : acc) | (w', v') <- edgesFrom (updateWeights g v c) v]
    
findPath :: Graph -> Vertex -> Path
findPath g v = reverse $ (\(_, y) -> y) (findPath' g v capacity 0 [])

step :: Graph -> Path -> (Maybe Vertex, Graph)
step g (v : v' : p) = (Just v', visitVertex (leaveVertex g v) v')
step g [v] = (Nothing, leaveVertex g v)
step g [] = (Nothing, g)

graph :: Graph
graph = 
    Graph [1, 2, 3] 
          (IntMap.fromList [(1, [(1, 2), (5, 3)]), (2, [(3, 3)])]) 
          (IntMap.fromList []) 
          [] 
          [3]


evacuate :: String -> MVar Graph -> Vertex -> IO ()
evacuate s mg v = do g <- takeMVar mg
                     p' <- return $ findPath g v
                     (v', g') <- return $ step g p'
                     putStrLn (s ++ ":   " ++ show v ++ " --> " ++ show v')
                     putMVar mg g'
                     case v' of Just x -> evacuate s mg x
                                Nothing -> putStrLn "You've made it out safely!"

data Layout = Rectangle Int Int Int Int Int Int
            | Wing Int Int Int Int Int
            | Floor [Layout] [(Int, Int)] [Int]
           
rectangle :: Int -> Layout
rectangle n = Rectangle n (n + 1) (n + 2) (n + 3) (n + 4) (n + 5)

wing :: Int -> Layout
wing n = Wing n (n + 1) (n + 2) (n + 3) (n + 4)

flooor :: Int -> Layout
flooor n = Floor [rectangle n, wing (n + 7), rectangle (n + 12)]
                 [(n + 4, n + 7), (n + 11, n + 15), (n + 6, n + 13)]
                 []

layout :: Layout -> [(Vertex, (Int, Vertex))]
layout (Rectangle n1 n2 n3 n4 n5 n6) = 
    [(n1, (1, n2)), (n2, (1, n3)), (n3, (1, n4)), 
     (n4, (1, n5)), (n5, (1, n6)), (n6, (1, n1))]
layout (Wing n1 n2 n3 n4 n5) =
    [(n1, (1, n2)), (n2, (1, n3)), (n3, (1, n4)), (n4, (1, n5))]
layout (Floor ls es xs) = 
    foldl (++) [] (map layout ls) ++ (map (\(x, y) -> (x, (1, y))) es)

bidirect :: [(Vertex, (Int, Vertex))] -> [(Vertex, (Int, Vertex))]
bidirect ((v, (w, u)) : es) = (v, (w, u)) : (u, (w, v)) : bidirect es
bidirect [] = []

obh :: Graph
obh = let g = Graph [0 .. 18] 
                    IntMap.empty 
                    IntMap.empty
                    [] 
                    [0] 
      in addEdges g ((bidirect . layout . flooor) 0)
                 

main :: IO ()
main = do g <- newMVar obh
          forkIO $ evacuate "1" g 1
          forkIO $ evacuate "2" g 2
          forkIO $ evacuate "3" g 6
          evacuate "4" g 6
