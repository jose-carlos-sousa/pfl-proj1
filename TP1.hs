import qualified Data.List
import Data.Array (Array, array)
import Data.Maybe (fromMaybe, mapMaybe)
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

type AdjList = [(City,[(City,Distance)])]


type Matrix = Data.Array.Array (Int,Int) (Maybe Distance)

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

roadMapToMatrix :: RoadMap -> Matrix
roadMapToMatrix roadMap = array bounds content
  where
    citiesList = cities roadMap 
    n = length citiesList
    bounds = ((0, 0), (n - 1, n - 1))
    cityIndex city = fromMaybe (-1) (Data.List.elemIndex city citiesList)
    initialContent = [((i, j), if i == j then Just 0 else Nothing) | i <- [0..n-1], j <- [0..n-1]]

    updatedContent = foldl (\acc (c1, c2, dist) ->
        let idx1 = cityIndex c1
            idx2 = cityIndex c2
        in if idx1 /= -1 && idx2 /= -1
           then ((idx1, idx2), Just dist) : ((idx2, idx1), Just dist) : acc
           else acc) initialContent roadMap

    content = Data.List.foldl' (\acc ((i,j), dist) -> ((i,j), dist) : acc) [] updatedContent

-- 1.

cities :: RoadMap -> [City]
cities roadMap = Data.List.nub ([ x | (x, _, _) <- roadMap ] ++ [ y | (_, y, _) <- roadMap ])

-- 2.

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap

-- 3.

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2 = 
    case filter (\(c1, c2, d) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap of
        [(c1, c2, d)] -> Just d
        []            -> Nothing
        _             -> Nothing

-- 4.

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city = [(c2, dist) | (c1, c2, dist) <- roadMap, c1 == city]

-- 5.

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

-- 6.

rome :: RoadMap -> [City]
rome roadMap =
    let adjList = roadMapToAdjList roadMap
        degrees = map (\(city, neighbors) -> (city, length neighbors)) adjList
        maxDegree = maximum (map snd degrees)
    in map fst (filter (\(_, degree) -> degree == maxDegree) degrees)

-- 7.

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap =
    let adjList = roadMapToAdjList roadMap
        allCities = Data.List.nub (concatMap (\(c1, c2, _) -> [c1, c2]) roadMap)
    in all (\city -> length (dfs adjList city []) == length allCities) allCities

dfs :: AdjList -> City -> [City] -> [City]
dfs adjList city visited
    | city `elem` visited = visited  -- Already visited this city
    | otherwise = foldl (\v (neighbor, _) -> dfs adjList neighbor v) (city : visited) neighbors
    where
        neighbors = fromMaybe [] (lookup city adjList)

-- 8.

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start end = filterByShortestDistance roadMap (findPaths roadMap start end [])

findPaths :: RoadMap -> City -> City -> Path -> [Path]
findPaths roadMap start end path
    | start == end = [path ++ [end]]
    | otherwise = concatMap (\(next, _) -> findPaths roadMap next end (path ++ [start])) neighbors
    where
        neighbors = adjacent roadMap start

shortestDistance :: RoadMap -> [Path] -> Maybe Distance
shortestDistance roadMap paths = 
    case distances of
        [] -> Nothing
        _ -> Just (minimum distances)
    where
        distances = mapMaybe (pathDistance roadMap) paths

filterByShortestDistance :: RoadMap -> [Path] -> [Path]
filterByShortestDistance roadMap paths = 
    case shortestDistance roadMap paths of
        Nothing -> []  -- No paths
        Just minDist -> filter (\p -> pathDistance roadMap p == Just minDist) paths

-- 9.

travelSales :: RoadMap -> Path
travelSales =undefined

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
