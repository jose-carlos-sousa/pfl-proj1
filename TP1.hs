import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]
type AdjList = [(City,[(City,Distance)])]
type Matrix = Data.Array.Array (Int,Int) (Maybe Distance)

-- Helper function to replace fromMaybe
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

-- Helper function to replace mapMaybe
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> case f x of
                                Just y  -> y : acc
                                Nothing -> acc) []

-- Helper comparison function to replace comparing fst
compareFst :: Ord a => (a, b) -> (a, b) -> Ordering
compareFst (x, _) (y, _) = compare x y

-- Helper function to convert an integer to binary string
toBinary :: Int -> String
toBinary 0 = "0"
toBinary n = reverse (toBinary' n)
  where
    toBinary' 0 = []
    toBinary' x = let (q, r) = x `divMod` 2
                  in (if r == 0 then '0' else '1') : toBinary' q

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
roadMapToMatrix roadMap = Data.Array.array bounds content
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

cities :: RoadMap -> [City]
cities roadMap = Data.List.nub ([ x | (x, _, _) <- roadMap ] ++ [ y | (_, y, _) <- roadMap ])

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2 = 
    case filter (\(c1, c2, d) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap of
        [(c1, c2, d)] -> Just d
        []            -> Nothing
        _             -> Nothing

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city = [(c2, dist) | (c1, c2, dist) <- roadMap, c1 == city]

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
rome roadMap =
    let adjList = roadMapToAdjList roadMap
        degrees = map (\(city, neighbors) -> (city, length neighbors)) adjList
        maxDegree = maximum (map snd degrees)
    in map fst (filter (\(_, degree) -> degree == maxDegree) degrees)

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap =
    let adjList = roadMapToAdjList roadMap
        allCities = cities roadMap
    in all (\city -> length (dfs adjList city []) == length allCities) allCities

dfs :: AdjList -> City -> [City] -> [City]
dfs adjList city visited
    | city `elem` visited = visited
    | otherwise = foldl (\v (neighbor, _) -> dfs adjList neighbor v) (city : visited) neighbors
    where
        neighbors = fromMaybe [] (lookup city adjList)

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
        Nothing -> []
        Just minDist -> filter (\p -> pathDistance roadMap p == Just minDist) paths


-- Creates an array to store paths for each (city, visited) state
createDPMatrix :: Int -> Data.Array.Array (Int, Integer) [Int]
createDPMatrix n = Data.Array.array ((0, 0), (n - 1, (1 `Data.Bits.shiftL` n) - 1)) 
                   [((i, j), []) | i <- [0 .. n - 1], j <- [0 .. (1 `Data.Bits.shiftL` n) - 1]]


-- The main TSP function that initializes the recursive DP solution
travelSales :: RoadMap -> Path
travelSales roadmap =
    let cityList = cities roadmap
        matrix = roadMapToMatrix roadmap
        n = length cityList
        dpMatrix = createDPMatrix n
        startMask = Data.Bits.setBit 0 0  -- start with only city 0 visited
        indicesPath = tspDP 0 startMask matrix n dpMatrix
    in map (cityList !!) indicesPath

-- Recursive function for TSP using DP with memoization
tspDP :: Int -> Integer -> Matrix -> Int -> Data.Array.Array (Int, Integer) [Int] -> [Int]
tspDP currentCity visited mat n dpMatrix =
    case dpMatrix Data.Array.! (currentCity, visited) of
        path@(_:_) -> path  -- Return cached path if it exists
        [] ->  -- Compute if not found
            let result = 
                    -- Base case: All cities visited, return to start if possible
                    if Data.Bits.popCount visited == n then  
                        case mat Data.Array.! (currentCity, 0) of
                            Just cost | cost >= 0 -> [currentCity, 0]
                            _ -> []
                    else
                        let -- Find next cities not yet visited
                            nextCities = [city | city <- [0 .. n - 1], 
                                        not (Data.Bits.testBit visited city)]
                            -- Calculate all possible paths to remaining cities
                            paths = [(totalCost, currentCity : restPath) |
                                    nextCity <- nextCities,
                                    let edgeCost = fromMaybe maxBound (mat Data.Array.! (currentCity, nextCity)),
                                    edgeCost < maxBound,
                                    let newVisited = Data.Bits.setBit visited nextCity,
                                    let restPath = tspDP nextCity newVisited mat n dpMatrix,
                                    not (null restPath),
                                    let pathCosts = [fromMaybe maxBound (mat Data.Array.! (a, b)) | 
                                                   (a, b) <- zip (currentCity : restPath) restPath],
                                    let totalCost = sum pathCosts]
                        in if null paths
                           then []
                           else snd (Data.List.minimumBy compareFst paths)
                
                -- Update dpMatrix with computed result for memoization
                newDpMatrix = dpMatrix Data.Array.// [((currentCity, visited), result)]
            in result

gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2),("0","2",2),("3","1",2)]
