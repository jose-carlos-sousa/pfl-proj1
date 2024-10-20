import qualified Data.List

import Data.Maybe (fromMaybe)
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

type AdjList = [(City,[(City,Distance)])]

addNeighbor :: City -> (City, Distance) -> AdjList -> AdjList
addNeighbor city neighbor [] = [(city, [neighbor])]
addNeighbor city neighbor ((c, neighbors):rest)
    | city == c  = (c, neighbor : neighbors) : rest
    | otherwise  = (c, neighbors) : addNeighbor city neighbor rest


roadMapToAdjList :: RoadMap -> AdjList
roadMapToAdjList [] = []
roadMapToAdjList ((city1, city2, dist):rest) =
    addNeighbor city2 (city1, dist) (addNeighbor city1 (city2, dist) adjList)
    where
        adjList = roadMapToAdjList rest

cities :: RoadMap -> [City]
cities roadMap = Data.List.nub ([ x | (x, _, _) <- roadMap ] ++ [ y | (_, y, _) <- roadMap ])


areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2 = 
    case filter (\(c1, c2, d) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap of
        [(c1, c2, d)] -> Just d
        []             -> Nothing
        _              -> Nothing


adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent = undefined

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadMap (city1:city2:rest) =
    case distance roadMap city1 city2 of
        Just dist ->
            case pathDistance roadMap (city2:rest) of
                Just restDist -> Just (dist + restDist)
                Nothing -> Nothing
        Nothing -> Nothing


rome :: RoadMap -> [City]
rome = undefined


dfs :: AdjList -> City -> [City] -> [City]
dfs adjList city visited
    | city `elem` visited = visited  -- Already visited this city
    | otherwise = foldl (\v (neighbor, _) -> dfs adjList neighbor v) (city : visited) neighbors
    where
        neighbors = fromMaybe [] (lookup city adjList)

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap =
    let adjList = roadMapToAdjList roadMap
        allCities = Data.List.nub (concatMap (\(c1, c2, _) -> [c1, c2]) roadMap)
    in all (\city -> length (dfs adjList city []) == length allCities) allCities

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined



-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
