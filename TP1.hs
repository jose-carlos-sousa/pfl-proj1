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

--Given a city and a neighbor with distance adds the neighbor to the city in the adjlist
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


areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap

-- Basically using list comprehension gets the element where c1 is first and c2 second or vice-versa
--This will be O(n) it goes over the whole list
--note to myself maybe using find would be better as it would stop earlier complexity wouldnt change in big O tho

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap city1 city2 =case [d | (c1, c2, d) <- roadmap , (city1 ==c1 && city2==c2) || (city1 ==c2 && city2==c1)] of
                                [d] -> Just d
                                [] ->Nothing
                                _   -> Nothing  -- In case there are multiple distances


adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city = [(c2, dist) | (c1, c2, dist) <- roadMap, c1 == city]++[(c1, dist) | (c1, c2, dist) <- roadMap, c2 == city]

--Basically for every pair in Path I check if exists if it does it is its distance plus the distance of rest of path if it doesnt exist is just Nothing
-- complexity is O(m*n) where m is path size and n is the size of the roadmap

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (c1:c2:rest) = case distance roadmap c1 c2 of
                                    Just d  -> case pathDistance roadmap (c2:rest) of
                                                    Just d2 -> Just (d + d2)
                                                    Nothing -> Nothing
                                    Nothing -> Nothing



rome :: RoadMap -> [City]
rome roadMap =
    let adjList = roadMapToAdjList roadMap
        degrees = map (\(city, neighbors) -> (city, length neighbors)) adjList
        maxDegree = maximum (map snd degrees)
    in map fst (filter (\(_, degree) -> degree == maxDegree) degrees)



-- Update the mask to include the given city
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
                                

type QueueEntry = (Distance, City, Path)

-- Insert into sorted queue (priority queue implementation)
insertQueue :: QueueEntry -> [QueueEntry] -> [QueueEntry]
insertQueue x [] = [x] 
insertQueue x (y:ys) = 
    let (d1, _, _) = x  
        (d2, _, _) = y 
    in if d1 <= d2 then 
        x : y : ys       -- If d1 is less than or equal to d2, place x in front of y
       else 
        y : insertQueue x ys  -- Otherwise, continue inserting x into the rest of the queue

-- shortestPath function using Dijkstra's algorithm
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

--Takes a roadMap and returns the corresponding TSP path
--Complexity of tsp is O(n*2^n) where n is the number of nodes in the graph
travelSales :: RoadMap -> Path
travelSales roadmap = map (cities roadmap !!) (map (\n -> n - 1) (snd (tsp (roadMapToMatrix roadmap))))

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTestBigGraph :: RoadMap
gTestBigGraph = [
    ("0", "1", 1), ("0", "2", 2), ("0", "3", 3), ("0", "4", 4), ("0", "5", 5),
    ("1", "2", 6), ("1", "3", 7), ("1", "4", 8), ("1", "5", 9), ("1", "6", 10),
    ("2", "3", 11), ("2", "4", 12), ("2", "5", 13), ("2", "6", 14), ("2", "7", 15),
    ("3", "4", 16), ("3", "5", 17), ("3", "6", 18), ("3", "7", 19), ("3", "8", 20),
    ("4", "5", 21), ("4", "6", 22), ("4", "7", 23), ("4", "8", 24), ("4", "9", 25),
    ("5", "6", 26), ("5", "7", 27), ("5", "8", 28), ("5", "9", 29), ("5", "10", 30),
    ("6", "7", 31), ("6", "8", 32), ("6", "9", 33), ("6", "10", 34), ("6", "11", 35),
    ("7", "8", 36), ("7", "9", 37), ("7", "10", 38), ("7", "11", 39), ("7", "12", 40),
    ("8", "9", 41), ("8", "10", 42), ("8", "11", 43), ("8", "12", 44), ("8", "13", 45),
    ("9", "10", 46), ("9", "11", 47), ("9", "12", 48), ("9", "13", 49), ("10", "11", 50),
    ("10", "12", 51), ("10", "13", 52), ("11", "12", 53), ("11", "13", 54), ("12", "13", 55),

    ("10", "14", 10), ("11", "15", 11), ("12", "16", 12), ("13", "17", 13), ("14", "15", 14),
    ("15", "16", 15), ("16", "17", 16), ("17", "18", 17), ("18", "19", 18), ("19", "20", 19),
    ("20", "21", 20), ("21", "22", 21), ("22", "23", 22), ("23", "24", 23), ("24", "25", 24),
    ("25", "26", 25), ("26", "27", 26), ("27", "28", 27), ("28", "29", 28), ("29", "30", 29),
    ("30", "31", 30), ("31", "32", 31), ("32", "33", 32), ("33", "34", 33), ("34", "35", 34),
    ("35", "36", 35), ("36", "37", 36), ("37", "38", 37), ("38", "39", 38), ("39", "40", 39),
    ("40", "41", 40), ("41", "42", 41), ("42", "43", 42), ("43", "44", 43), ("44", "45", 44),
    ("45", "46", 45), ("46", "47", 46), ("47", "48", 47), ("48", "49", 48), ("49", "50", 49),

    ("50", "51", 10), ("51", "52", 11), ("52", "53", 12), ("53", "54", 13), ("54", "55", 14),
    ("55", "56", 15), ("56", "57", 16), ("57", "58", 17), ("58", "59", 18), ("59", "60", 19),
    ("60", "61", 20), ("61", "62", 21), ("62", "63", 22), ("63", "64", 23), ("64", "65", 24),
    ("65", "66", 25), ("66", "67", 26), ("67", "68", 27), ("68", "69", 28), ("69", "70", 29),
    ("70", "71", 30), ("71", "72", 31), ("72", "73", 32), ("73", "74", 33), ("74", "75", 34),
    ("75", "76", 35), ("76", "77", 36), ("77", "78", 37), ("78", "79", 38), ("79", "80", 39),
    ("80", "81", 40), ("81", "82", 41), ("82", "83", 42), ("83", "84", 43), ("84", "85", 44),
    ("85", "86", 45), ("86", "87", 46), ("87", "88", 47), ("88", "89", 48), ("89", "90", 49),
    ("90", "91", 50), ("91", "92", 51), ("92", "93", 52), ("93", "94", 53), ("94", "95", 54),
    ("95", "96", 55), ("96", "97", 56), ("97", "98", 57), ("98", "99", 58), ("99", "0", 59)]