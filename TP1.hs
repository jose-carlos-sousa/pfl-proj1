import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

newTable ::(Data.Array.Ix b) =>[(b, a)] -> Table a b
findTable ::(Data.Array.Ix b) =>Table a b -> b -> a
updTable ::(Data.Array.Ix b) => (b,a) -> Table a b -> Table a b

newtype Table a b = Tbl (Data.Array.Array b a)
    deriving Show


newTable l = Tbl (Data.Array.array (lo, hi) l)
  where
    indices = map fst l
    lo      = minimum indices
    hi      = maximum indices

findTable (Tbl a) i = a Data.Array.! i

updTable p@(i,x) (Tbl a) = Tbl (a Data.Array.// [p])

dynamic :: Data.Array.Ix coord => (Table entry  coord -> coord -> entry) -> (coord,coord) ->(Table entry coord)
dynamic compute bnds = t
    where t = newTable (map ( \coord -> ( coord , compute t coord) ) (Data.Array.range bnds ) )

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

cityIndex roadMap city = fromMaybe (-1) (Data.List.elemIndex city (cities roadMap))

roadMapToMatrix :: RoadMap -> Matrix
roadMapToMatrix roadMap = mkGraph False bounds edges
  where 
    citi = cities roadMap
    bounds = (1, length citi)  -- Adjust bounds for one-based indexing
    edges = [((cityIndex roadMap c1) + 1, (cityIndex roadMap c2) + 1, d) | (c1, c2, d) <- roadMap]

   

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


dfs :: RoadMap -> AdjList -> City -> Integer -> Integer
dfs roadMap adjList currentCity currentMask =
    if Data.Bits.testBit currentMask (cityIndex roadMap currentCity)
    then 0 -- Already visited
    else neighborResults Data.Bits..|. (1 `Data.Bits.shiftL` (cityIndex roadMap currentCity)) -- Include current city in the result
  where
    updatedMask = Data.Bits.setBit currentMask (cityIndex roadMap currentCity)
    neighbors = [ city | (city, distance) <- adjacent roadMap currentCity ]
    neighborResults = foldl (\acc neiCity -> acc Data.Bits..|. dfs roadMap adjList neiCity updatedMask) 0 neighbors

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

-- Modified shortestPath using Dijkstra's algorithm
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start end 
    | start == end = [[start]]
    | otherwise = dijkstra roadMap end [(0, start, [start])] [] []


-- Main Dijkstra's algorithm implementation
dijkstra :: RoadMap -> City -> [QueueEntry] -> [City] -> [Path] -> [Path]
dijkstra _ _ [] _ paths = paths
dijkstra roadMap end ((totalDist, curr, currPath):queue) visited paths
    | curr == end = 
        case paths of
            [] -> dijkstra roadMap end queue visited [currPath]
            (p:_) -> case pathDistance roadMap p of
                Just minDist 
                    | totalDist == minDist -> dijkstra roadMap end queue visited (currPath:paths)
                    | totalDist < minDist -> dijkstra roadMap end queue visited [currPath]
                    | otherwise -> dijkstra roadMap end queue visited paths
                Nothing -> dijkstra roadMap end queue visited [currPath]
    | curr `elem` visited = dijkstra roadMap end queue visited paths
    | otherwise = 
        let neighbors = [(next, dist) | (next, dist) <- adjacent roadMap curr,
                                      not (next `elem` visited)]
            newQueue = foldl (\q (next, dist) -> 
                            insertQueue (totalDist + dist, next, currPath ++ [next]) q)
                            queue neighbors
        in dijkstra roadMap end newQueue (curr:visited) paths



bndsFib :: Int -> (Int, Int)
bndsFib n = (0, n)

-- Compute Fibonacci using the provided table
compFib :: Table Int Int -> Int -> Int
compFib t i
    | i == 0    = 0
    | i == 1    = 1
    | otherwise = findTable t (i - 1) + findTable t (i - 2)

-- Fibonacci function using dynamic programming
fib :: Int -> Int
fib n = findTable t n
  where t = dynamic compFib (bndsFib n)


mkGraph dir bnds@(1,u) es
    = emptyArray Data.Array.// ([((x1,x2), Just w) | (x1,x2,w) <-es]++ if dir then [] else [((x2,x1), Just w) | (x1 ,x2 ,w) <-es , x1 /=x2])
    where emptyArray = Data.Array.array ((1,1),(u,u)) [((x1,x2), Nothing) | x1 <- Data.Array.range bnds, x2 <- Data.Array.range bnds]



nodes g = Data.Array.range (1,u ) where ((1, _), (u,_)) = Data.Array.bounds g

weight x y g = let w = g Data.Array.! (x, y) in
    case w of
        Just weightValue -> weightValue
        Nothing -> maxBound


type Set = Integer  

emptySet :: Set
emptySet = 0

setEmpty :: Set -> Bool
setEmpty n = n == 0


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

-- Converts a set to a list
set2List :: Set -> [Int]
set2List s = s2l s 0
    where s2l 0 _ = []
          s2l n i | odd n   = i : s2l (n `div` 2) (i + 1)
                   | otherwise = s2l (n `div` 2) (i + 1)

type TspCoord = (Int, Set)
type TspEntry = (Int, [Int]) -- An entry in the table is a tuple consisting of the value c (of type Int) and the corresponding shortest path (a list of vertices).

adjacentNodes :: Matrix -> Int -> [Int]
adjacentNodes g v1 = [v2 | v2 <- nodes g, (g Data.Array.! (v1, v2)) /= Nothing]

compTsp :: Matrix -> Int -> Table TspEntry TspCoord -> TspCoord -> TspEntry
compTsp g n a (i, k)
  | setEmpty k = 
      let finalEdgeWeight = weight i n g
      in if finalEdgeWeight < maxBound 
         then (finalEdgeWeight, [i, n])
         else (maxBound, [])
  | otherwise = 
      let paths = [(totalCost, i : restPath) |
                   j <- set2List k,
                   let edgeWeight = weight i j g,
                   edgeWeight < maxBound,
                   let (subCost, restPath) = findTable a (j, delSet j k),
                   not (null restPath),
                   let totalCost = edgeWeight + subCost]
      in if null paths
           then (maxBound, [])
           else minimum paths

bndsTsp :: Int -> ((Int, Set), (Int, Set))
bndsTsp n = ((1, emptySet), (n, fullSet (n - 1)))

tsp :: Matrix -> (Int, [Int])
tsp g
    | n == 0    = (0, [])         
    | n == 1    = (0, [0])        
    | otherwise = findTable t (n, fullSet (n - 1))
    where
        n = length (nodes g)
        t = dynamic (compTsp g n) (bndsTsp n)

travelSales :: RoadMap -> Path
travelSales roadmap = map (cities roadmap !!) (map (\n -> n - 1) (snd (tsp (roadMapToMatrix roadmap))))


gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0", "1", 5), ("1", "2", 10), ("2", "0", 15)]

gTest4 :: RoadMap -- unconnected graph
gTest4 = [("0", "1", 5), ("0", "2", 21), ("0", "3", 38), ("0", "4", 46), ("0", "5", 40),
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
main :: IO ()
main = do
    let citi = cities gTest1
    let bounds = (1, length citi)
    let edges = [(cityIndex gTest1 c1 + 1, cityIndex gTest1 c2 + 1, d) | (c1, c2, d) <- gTest1]
    print $ "Cities: " ++ show citi
    print $ "Bounds: " ++ show bounds
    print $ "Edges: " ++ show edges
    let graph = mkGraph False bounds edges
    print $ "Graph: " ++ show graph
    let (cost, path) = tsp graph  -- Call the TSP function
    putStrLn $ "Minimum cost: " ++ show cost ++ ", Path: " ++ show path