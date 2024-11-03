import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]
type AdjList = [(City,[(City,Distance)])]
type Matrix = Data.Array.Array (Int,Int) (Maybe Distance)
type TspCoord = (Int, Set)
type TspEntry = (Int, [Int]) -- An entry in the table is a tuple consisting of the value c (of type Int) and the corresponding shortest path (a list of vertices).

--generic table definition provided by the book RL99
newtype Table a b = Tbl (Data.Array.Array b a)
    deriving Show

newTable ::(Data.Array.Ix b) =>[(b, a)] -> Table a b
newTable l = Tbl (Data.Array.array (lo, hi) l)
  where
    indices = map fst l
    lo      = minimum indices
    hi      = maximum indices

--Given a table and a coord returns the value at that coord
findTable ::(Data.Array.Ix b) =>Table a b -> b -> a
findTable (Tbl a) i = a Data.Array.! i

--Given a function that maps coord to entries and a range of coords returns a table with the values of the function at each coord
dynamic :: Data.Array.Ix coord => (Table entry  coord -> coord -> entry) -> (coord,coord) ->(Table entry coord)
dynamic compute bnds = t
    where t = newTable (map ( \coord -> ( coord , compute t coord) ) (Data.Array.range bnds ) )


-- Helper function to handle Maybe values
-- Given a default value and a Maybe value, returns the default if the input is Nothing,
-- or the value inside Just if it exists.
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

-- Adds a neighbor with a given distance to a city in the adjacency list
-- If the city does not exist in the adjacency list, it creates a new entry.
addNeighbor :: City -> (City, Distance) -> AdjList -> AdjList
addNeighbor city neighbor [] = [(city, [neighbor])]
addNeighbor city neighbor ((c, neighbors):rest)
    | city == c  = (c, neighbor : neighbors) : rest
    | otherwise  = (c, neighbors) : addNeighbor city neighbor rest

--Given a roadmap returns the corresponding adjlist 
roadMapToAdjList :: RoadMap -> AdjList
roadMapToAdjList [] = []
roadMapToAdjList ((city1, city2, dist):rest) =
    addNeighbor city2 (city1, dist) (addNeighbor city1 (city2, dist) adjList)
    where
        adjList = roadMapToAdjList rest

--Given a roadmap and a city returns the index of the city in the roadmap
cityIndex :: RoadMap -> City -> Int
cityIndex roadMap city = fromMaybe (-1) (Data.List.elemIndex city (cities roadMap))

--Given a roadmap returns the corresponding matrix
roadMapToMatrix :: RoadMap -> Matrix
roadMapToMatrix roadMap = mkGraph False bounds edges
  where 
    citi = cities roadMap
    bounds = (1, length citi)  -- Adjust bounds for one-based indexing
    edges = [((cityIndex roadMap c1) + 1, (cityIndex roadMap c2) + 1, d) | (c1, c2, d) <- roadMap]

--Given a bool that says if the graph is directed, a bound and a list of edges returns the corresponding matrix
mkGraph :: Bool -> (Int, Int) -> [(Int, Int, Distance)] -> Matrix
mkGraph dir bnds@(1,u) es
    = emptyArray Data.Array.// ([((x1,x2), Just w) | (x1,x2,w) <-es]++ if dir then [] else [((x2,x1), Just w) | (x1 ,x2 ,w) <-es , x1 /=x2])
    where emptyArray = Data.Array.array ((1,1),(u,u)) [((x1,x2), Nothing) | x1 <- Data.Array.range bnds, x2 <- Data.Array.range bnds]

--Given a matrix returns the nodes
nodes :: Matrix -> [Int]
nodes g = Data.Array.range (1,u ) where ((1, _), (u,_)) = Data.Array.bounds g

--Given 2 nodes and a matrix returns the weight of the edge between them
weight :: Int -> Int -> Matrix -> Distance
weight x y g = let w = g Data.Array.! (x, y) in
    case w of
        Just weightValue -> weightValue
        Nothing -> maxBound

--Given a matrix and a node returns the adjacent nodes
adjacentNodes :: Matrix -> Int -> [Int]
adjacentNodes g v1 = [v2 | v2 <- nodes g, (g Data.Array.! (v1, v2)) /= Nothing]

--Set type defined in RL99
type Set = Integer  

--creates an empty set
emptySet :: Set
emptySet = 0

--checks if a set is empty
setEmpty :: Set -> Bool
setEmpty n = n == 0

--creates a full set
fullSet :: Int -> Set
fullSet n = 2 ^ (n + 1) - 2

-- Adds a bit to a mask
addSet :: Int -> Set -> Set
addSet i s = d_ * e + m
    where (d, m) = divMod s e
          e = 2 ^ i
          d_ = if odd d then d else d + 1

-- Removes a bit from a mask
delSet :: Int -> Set -> Set
delSet i s = d_ * e + m
    where (d, m) = divMod s e
          e = 2 ^ i
          d_ = if odd d then d - 1 else d

-- Converts a set to a list with the setted elements
set2List :: Set -> [Int]
set2List s = s2l s 0
    where s2l 0 _ = []
          s2l n i | odd n   = i : s2l (n `div` 2) (i + 1)
                   | otherwise = s2l (n `div` 2) (i + 1)


-- Given a roadmap returns unique cities
-- Does this by using nub which returns unique elements in a List
-- In this case the list used is the concatenation of one with the elements where city is first and another where it is second
-- The overall complexity will be O(n²) because of nub

cities :: RoadMap -> [City]
cities roadmap = Data.List.nub ([x  | (x,_,_) <- roadmap] ++ [y  | (_,y,_) <- roadmap])

-- Given a roadmap and two cities returns if they are adjacent
-- This will be O(n) where n is the size of the roadmap

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap

-- Basically using list comprehension gets the element where c1 is first and c2 second or vice-versa
-- This will be O(n) it goes over the whole list

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap city1 city2 =case [d | (c1, c2, d) <- roadmap , (city1 ==c1 && city2==c2) || (city1 ==c2 && city2==c1)] of
                                [d] -> Just d
                                [] ->Nothing
                                _   -> Nothing 

-- Given a roadmap and a city returns the adjacent cities and their distances
-- This will be O(n) + O(n) which is just O(n) where n is the size of the roadmap

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city = [(c2, dist) | (c1, c2, dist) <- roadMap, c1 == city]++[(c1, dist) | (c1, c2, dist) <- roadMap, c2 == city]

-- Basically for every pair in Path it checks its distance and adds the distance of the rest of the path if that edge doesn't exist then just returns Nothing
-- complexity is O(m*n) where m is path size and n is the size of the roadmap

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (c1:c2:rest) = case distance roadmap c1 c2 of
                                    Just d  -> case pathDistance roadmap (c2:rest) of
                                                    Just d2 -> Just (d + d2)
                                                    Nothing -> Nothing
                                    Nothing -> Nothing

-- Given a roadmap returns the cities with the most neighbours (highest degree)
-- We first convert the roadmap to an adjacency list for easy degree calculation
-- Conversion is O(m*n) where n is the size of the roadmap and m is the number of edges
-- Total complexity is O(m*n + n) = O(m*n)

rome :: RoadMap -> [City]
rome roadMap =
    let adjList = roadMapToAdjList roadMap
        degrees = map (\(city, neighbors) -> (city, length neighbors)) adjList
        maxDegree = maximum (map snd degrees)
    in map fst (filter (\(_, degree) -> degree == maxDegree) degrees)



-- Update the mask to include the given city
-- Given a roadmap, a mask, and a city, adds the bit at index of city to mask
updateMask :: RoadMap -> Integer -> City -> Integer
updateMask roadMap mask city = mask Data.Bits..|. (1 `Data.Bits.shiftL` (cityIndex roadMap city))

-- Given a roadaMap, an adjacency list, a current city, a current mask returns the mask with the cities visited
-- Dfs is O(V+E) goes through all the cities and all the edges
dfs :: RoadMap -> AdjList -> City -> Integer -> Integer
dfs roadMap adjList currentCity currentMask =
    if Data.Bits.testBit currentMask (cityIndex roadMap currentCity)
    then 0 -- Already visited
    else neighborResults Data.Bits..|. (1 `Data.Bits.shiftL` (cityIndex roadMap currentCity)) -- Include current city in the result
  where
    updatedMask = Data.Bits.setBit currentMask (cityIndex roadMap currentCity)
    neighbors = [ city | (city, distance) <- adjacent roadMap currentCity ]
    neighborResults = foldl (\acc neiCity -> acc Data.Bits..|. dfs roadMap adjList neiCity updatedMask) 0 neighbors

-- Given a roadmap it return a bool that says if the graph is strongly connected
-- Time complexity will be O(n²) n calls to O(n+e) dfs
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap = all (\city -> Data.Bits.popCount (dfs roadMap adjList city 0) == length allCities) allCities
                                where adjList = roadMapToAdjList roadMap
                                      allCities = cities roadMap
                                

type QueueEntry = (Distance, City, Path) -- Entry in the priority queue

-- Insert into sorted queue (priority queue implementation using a list)
--Given new entry and current PQ returns new PQ with the entry in the correct place
insertQueue :: QueueEntry -> [QueueEntry] -> [QueueEntry]
insertQueue x [] = [x] 
insertQueue x (y:ys) = 
    let (d1, _, _) = x  
        (d2, _, _) = y 
    in if d1 <= d2 then 
        x : y : ys
       else 
        y : insertQueue x ys

-- shortestPath function using Dijkstra's algorithm with adjacency list
-- The complexity of this function is O((V + E) * V) where V is the number of cities and E is the number of edges
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start end 
    | start == end = [[start]]
    | otherwise =
        let adjList = roadMapToAdjList roadMap
        in dijkstra adjList end [(0, start, [start])] [] []

-- Main Dijkstra's algorithm implementation
dijkstra :: AdjList -> City -> [QueueEntry] -> [City] -> [Path] -> [Path]
dijkstra _ _ [] _ paths = paths
dijkstra adjList end ((totalDist, curr, currPath):queue) visited paths
    | curr == end = 
        case paths of
            [] -> dijkstra adjList end queue visited [currPath]
            (p:_) ->
                if totalDist == pathDistanceAdj p adjList
                then dijkstra adjList end queue visited (currPath : paths)
                else if totalDist < pathDistanceAdj p adjList
                     then dijkstra adjList end queue visited [currPath]
                     else dijkstra adjList end queue visited paths
    | curr `elem` visited = dijkstra adjList end queue visited paths
    | otherwise =
        let neighbours = fromMaybe [] (Data.List.lookup curr adjList)
            newQueue = foldl (\q (next, dist) ->
                insertQueue (totalDist + dist, next, currPath ++ [next]) q) queue neighbours
        in dijkstra adjList end newQueue (curr : visited) paths

-- Helper function to work on AdjList instead of RoadMap
-- Given a path and an adjList calculates total path distance
pathDistanceAdj :: Path -> AdjList -> Distance
pathDistanceAdj [] _ = 0
pathDistanceAdj [_] _ = 0
pathDistanceAdj (c1:c2:rest) adjList =
    case Data.List.lookup c1 adjList >>= lookup c2 of
        Just dist -> dist + pathDistanceAdj (c2:rest) adjList

--takes matrix ,last node, table and table coord (with cur node and state)  and returns the tspentry with the cost and path of the tsp at that state
compTsp :: Matrix -> Int -> Table TspEntry TspCoord -> TspCoord -> TspEntry
compTsp g n a (i, k)
    |setEmpty k =if (weight i n g) < maxBound then (weight i n g, [i, n]) else (maxBound, []) -- If the set is empty, return the weight of the edge from i to n
    |otherwise = if null paths then (maxBound, []) else minimum paths -- Otherwise, find the minimum path from i to n
        where
            paths = [(totalCost, i : restPath) |  -- For each j in k, find the minimum path from j to n and add the weight of the edge from i to j
                    j <- set2List k,
                    let edgeWeight = weight i j g,
                    edgeWeight < maxBound,
                    let (subCost, restPath) = findTable a (j, delSet j k),
                    not (null restPath),
                    let totalCost = edgeWeight + subCost]

-- takes n (size of matrix) and returns the bounds for the memo table
bndsTsp :: Int -> ((Int, Set), (Int, Set))
bndsTsp n = ((1, emptySet), (n, fullSet (n - 1)))

--Takes a matrix and returns a tuple with the cost of the tsp and the path of the tsp
tsp :: Matrix -> (Int, [Int])
tsp g
    | n == 0    = (0, [])         -- If there are no nodes, return 0
    | n == 1    = (0, [0])        -- If there is only one node, return 0
    | otherwise = findTable t (n, fullSet (n - 1)) --- Otherwise, find the minimum path from n to all the other nodes
    where
        n = length (nodes g)
        t = dynamic (compTsp g n) (bndsTsp n) -- Create a dynamic table to store the results of the subproblems

-- Takes a roadMap and returns the corresponding TSP path
-- Complexity of tsp is O(n²*2^n) where n is the number of nodes in the graph
-- For each entry in the table (n*2^n entries) it requires up to n operations to compute
travelSales :: RoadMap -> Path
travelSales roadmap = map (cities roadmap !!) (map (\n -> n - 1) (snd (tsp (roadMapToMatrix roadmap))))

-- Some graphs to test your work

--original graphs

gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]


--other graphs

gTest4 :: RoadMap 
gTest4 = [("1", "2", 5), ("2", "3", 10), ("3", "1", 15), ("1", "4", 20), ("4", "2", 25)]

gTest5 :: RoadMap -- Dense graph
gTest5 = [("0", "1", 5), ("0", "2", 21), ("0", "3", 38), ("0", "4", 46), ("0", "5", 40),
           ("0", "6", 27), ("0", "7", 114), ("0", "8", 43), ("0", "9", 26), ("0", "10", 34),
           ("1", "2", 25), ("1", "3", 41), ("1", "4", 48), ("1", "5", 43), ("1", "6", 29),
           ("1", "7", 118), ("1", "8", 44), ("1", "9", 24), ("1", "10", 38),
           ("2", "3", 39), ("2", "4", 33), ("2", "5", 21), ("2", "6", 18), ("2", "7", 94),
           ("2", "8", 37), ("2", "9", 31), ("2", "10", 15),
           ("3", "4", 72), ("3", "5", 57), ("3", "6", 57), ("3", "7", 112), ("3", "8", 75),
           ("3", "9", 63), ("3", "10", 51),
           ("4", "5", 17), ("4", "6", 20), ("4", "7", 83), ("4", "8", 16), ("4", "9", 34),
           ("4", "10", 22),
           ("5", "6", 19), ("5", "7", 78), ("5", "8", 29), ("5", "9", 38), ("5", "10", 6),
           ("6", "7", 96), ("6", "8", 19), ("6", "9", 19), ("6", "10", 18),
           ("7", "8", 99), ("7", "9", 115), ("7", "10", 81),
           ("8", "9", 23), ("8", "10", 32),
           ("9", "10", 36)]

gTest6 :: RoadMap --Star graph
gTest6 = [("1", "2", 3), ("1", "3", 5), ("1", "4", 7), ("1", "5", 2)]


gTest7 :: RoadMap
gTest7 = []

