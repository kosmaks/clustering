import Data.List
import Control.Lens
import Control.Monad
import Graphics.Gnuplot.Simple
import System.Environment

-- Vectors

data Vec3 = Vec3 Float Float Float deriving (Show, Read)

parseList :: String -> [Vec3]
parseList src = map ((read :: String -> Vec3) . 
                     ("Vec3 "++)) $ lines src

distance :: Vec3 -> Vec3 -> Float
distance (Vec3 x0 y0 _) (Vec3 x1 y1 _) = sqrt ((x0 - x1) ** 2 + (y0 - y1) ** 2)

-- Clusters and shit

type Cluster a = [a]
data DistanceMatrix a = DM { clusters :: [Cluster a], 
                             distanceMap :: [[Float]] }
data LinkType = Single | Complete | UPGMA | WPGMA
                deriving (Show, Read)

instance Show a => Show (DistanceMatrix a) where
  show (DM clus dist) = 
    let header = foldl ((++) . (++"\t")) "" $ map show clus
    in "CLUS:\t" ++ header ++ "\n"

createDM :: [Vec3] -> DistanceMatrix Vec3
createDM xs = DM (map (:[]) xs) (map (\x -> map (distance x) xs) xs)

linker :: LinkType -> Float -> Float -> Float -> Float -> Float

linker Single tu tv uw vw = min uw vw
linker Complete tu tv uw vw = max uw vw
linker UPGMA tu tv uw vw = (tu * uw + tv * vw) / (tu + tv)
linker WPGMA tu tv uw vw = (uw + vw) / 2

mutate :: LinkType -> DistanceMatrix a -> DistanceMatrix a
mutate lt (DM clus dist) =
  let (x, y, a) = findMinND dist
      ely = clus !! y
      elx = clus !! x
      tu = fromIntegral $ length ely
      tv = fromIntegral $ length elx

      newClus = deleteRow x $ set (element y) (elx ++ ely) clus
      distVal uw vw = linker lt tu tv uw vw
      
      mapper i j v
        | i == j || i == x || j == x = 0
        | i /= y && j /= y = v
        | i == y = distVal (dist !! y !! j) (dist !! x !! j)
        | j == y = distVal (dist !! y !! i) (dist !! x !! i)
      
      zipped = zip [0..] $ map (zip [0..]) dist
      newDist = map (\(i, y) -> map (\(j, x) -> mapper i j x) y) zipped
  in DM newClus (deleteColumn x $ deleteRow x newDist)

mutateSeq :: LinkType -> Int -> DistanceMatrix a -> DistanceMatrix a
mutateSeq lt 0 dm = dm
mutateSeq lt n dm@(DM (x:[]) _) = dm
mutateSeq lt n dm = mutateSeq lt (n-1) $ mutate lt dm

-- Matrix operations

deleteRow :: Int -> [a] -> [a]
deleteRow i m = (\(x, y:z) -> x ++ z) $ splitAt i m

deleteColumn :: Int -> [[a]] -> [[a]]
deleteColumn i m = map (deleteRow i) m

-- Find min value with coordinates of column and row

findMinND :: Ord a => [[a]] -> (Int, Int, a)
findMinND m = 
  let windex = rzip $ map rzip m       -- appeneded indexes
      nd = map removeDiag windex       -- removed diagonal
      minRes = minimum $ map minCol nd -- find minimum
  in format minRes 
  where rzip x = zip x [0..]
        minCol (list, i) = (minimum list, i)
        removeDiag (list, i) = ((filter (\(_, j) -> j /= i) list), i)
        format ((a, x), y) = (x, y, a)

-- Output

plotClusters :: [Cluster Vec3] -> IO ()
plotClusters xs = plotPaths [] $ map (map to2) xs
  where to2 (Vec3 x y z) = (x, y)

data Options = Options {
                 mutations :: Int,
                 linkType :: LinkType
               } | Help

parseArgs :: [[Char]] -> Options
parseArgs (m:lt:xs) = Options (read m :: Int) (read lt :: LinkType)
parseArgs (m:[]) = parseArgs $ [m, "Single"]
parseArgs ([]) = parseArgs $ ["0", "Single"]

main :: IO ()
main = do
  args <- getArgs 
  content <- getContents

  let options = parseArgs args
      list = parseList content
      dm = createDM list

  putStrLn $ "Read " ++ (show $ length list) ++ " vertices."
  putStrLn $ "Doing " ++ (show $ mutations options) ++ " mutations (first parameter)."
  putStrLn $ "Mutation type: " ++ (show $ linkType options) ++ 
             " (posible values: Single, Complete, UPGMA, WPGMA)."

  plotClusters $ clusters $ mutateSeq (linkType options) (mutations options) dm
