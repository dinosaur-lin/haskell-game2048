module Lib
    ( start
    ) where

import qualified Data.Vector as V
import System.Random
import Control.Monad (replicateM)

convertNum2Str :: Int -> Int -> [Char]
convertNum2Str num len = let numStr = show num in
                         let numSpace = len - length numStr in
    (foldl (\x y -> x ++ y) "" (replicate numSpace " ")) ++ numStr

convertVector2Str :: Int -> (V.Vector Int) -> [Char]
convertVector2Str len vec = let vecStr = fmap (\x -> convertNum2Str x len) vec in
                            foldl (\x y -> x ++ y) "" vecStr

convertMatrix2Str :: Int -> (V.Vector (V.Vector Int)) -> [Char]
convertMatrix2Str len mat = let mstr = fmap (\x -> convertVector2Str len x) mat in
                              foldl (\x y -> x ++ "\n" ++ y) "" mstr

printMatrix :: Int -> (V.Vector (V.Vector Int)) -> IO ()
printMatrix len mat = putStrLn (convertMatrix2Str len mat)

randomLoc :: Int -> Int -> StdGen -> ((Int, Int), StdGen)
randomLoc w h g = let (x, g1) = (randomR (0, w-1) g) in
                  let (y, g2) = (randomR (0, h-1) g1) in
                    ((x, y), g2)

updateMatrix :: (V.Vector (V.Vector Int)) -> (Int, Int) -> Int -> (V.Vector (V.Vector Int))
updateMatrix m loc v = V.update m (V.fromList [(fst loc, rv)])
  where rv = V.update (m V.! (fst loc)) (V.fromList [(snd loc, v)])

createInitMatrix :: Int -> Int -> StdGen -> (V.Vector (V.Vector Int))
createInitMatrix w h g = let initM = (V.generate h (\n -> V.replicate w 0)) in
                             updateMatrix initM loc 2
                             where loc = fst (randomLoc w h g)

start :: IO ()
start = do
  g <- getStdGen
  printMatrix 5 (createInitMatrix 4 4 g)
