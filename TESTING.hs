
import Data.Array
import Debug.Trace

newTable ::(Ix b) =>[(b, a)] -> Table a b
findTable ::(Ix b) =>Table a b -> b -> a
updTable ::(Ix b) => (b,a) -> Table a b -> Table a b

newtype Table a b = Tbl (Array b a)
    deriving Show


newTable l = Tbl (Data.Array.array (lo, hi) l)
  where
    indices = map fst l
    lo      = minimum indices
    hi      = maximum indices

findTable (Tbl a) i = a Data.Array.! i

updTable p@(i,x) (Tbl a) = Tbl (a Data.Array.// [p])

dynamic :: Ix coord => (Table entry  coord -> coord -> entry) -> (coord,coord) ->(Table entry coord)
dynamic compute bnds = t
    where t = newTable (map ( \coord -> ( coord , compute t coord) ) (Data.Array.range bnds ) )

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
type Distance = Int
type Graph = Array (Int , Int) (Maybe Distance)



mkGraph dir bnds@(1,u) es
    = emptyArray // ([((x1,x2), Just w) | (x1,x2,w) <-es]++ if dir then [] else [((x2,x1), Just w) | (x1 ,x2 ,w) <-es , x1 /=x2])
    where emptyArray = array ((1,1),(u,u)) [((x1,x2), Nothing) | x1 <- range bnds, x2 <- range bnds]

adjacent g v1 = [v2 | v2 <- nodes g , (g!(v1,v2)) /= Nothing]

nodes g = range (1,u ) where ((1, _), (u,_)) = bounds g

weight x y g = let w = g Data.Array.! (x, y) in
    trace ("Looking up weight for edge (" ++ show x ++ ", " ++ show y ++ "): " ++ show w) $
    case w of
        Just weightValue -> weightValue
        Nothing -> maxBound


type Set = Int

emptySet = 0 

setEmpty n = n ==0


fullSet n | (n>=0) && (n<=maxSet)  = 2 ^ (n+ 1 ) -2
          |  otherwise = error ( " fullset : illegal set = " ++ show n)

--adiciona bit a uma mascara
addSet i s = d_*e+m
    where(d,m) = divMod s e
         e = 2^i
         d_ = if odd d then d   else d+1

--remove bit de uma mascara
delSet i s = d_ * e + m
  where (d, m) = divMod s e
        e = 2 ^ i
        d_ = if odd d then d - 1 else d

set2List s = s2l s 0
        where   s2l 0 _ = []
                s2l n i | odd n   = i : s2l (n `div` 2) (i+1)
                        | otherwise = s2l (n `div` 2) (i+1)

maxSet = truncate ( logBase 2 ( fromIntegral (maxBound :: Int ) ) ) - 1


type TspCoord = ( Int , Set  )
type TspEntry = ( Int , [Int] ) --An entry in the table is a tuple which consists of the value c (of type Int) and the corresponding shortest path (a list of vertices). 



adjacentNodes :: Graph -> Int -> [Int]
adjacentNodes g v1 = [v2 | v2 <- nodes g, (g Data.Array.! (v1, v2)) /= Nothing]

compTsp :: Graph -> Int -> Table TspEntry TspCoord -> TspCoord -> TspEntry
compTsp g n a (i,k)
  | setEmpty k = 
      let finalEdgeWeight = weight i n g
      in if finalEdgeWeight < maxBound 
         then (finalEdgeWeight, [i,n])
         else (maxBound, [])
  | otherwise =let
     paths = [(totalCost, i : restPath) |
             j <- set2List k,
             let edgeWeight = weight i j g,
             edgeWeight < maxBound,
             let (subCost, restPath) = findTable a (j, delSet j k),
             not (null restPath),
             let totalCost = edgeWeight + subCost]
        in if null paths
           then (maxBound, [])
           else minimum paths
   

bndsTsp :: Int -> ( ( Int , Set ) , ( Int , Set ) )
bndsTsp n = ( ( 1 , emptySet ) , (n , fullSet n) )

tsp :: Graph  -> (Int, [Int])
tsp g = findTable t (n, fullSet (n - 1))
    where
        n = length (nodes g)
        t = dynamic (compTsp g n) (bndsTsp n)

main :: IO ()
main = do
    let bounds = (1, 5)
    let edges =[(1, 2, 5), (2, 3, 10), (3, 1, 15),(4, 5, 15)]
    let graph = mkGraph False bounds edges
    print graph
    let (cost, path) = tsp graph  -- Call the TSP function
    putStrLn $ "Minimum cost: " ++ show cost ++ ", Path: " ++ show path