module Lib
    ( start
    ) where

import qualified Data.Vector as V

convertNum2Str :: Int -> Int -> [Char]
convertNum2Str num len = let numStr = show num in
                         let numSpace = len - length numStr in
    (foldl (\x y -> x ++ y) "" (replicate numSpace " ")) ++ numStr

convertVector2Str :: Int -> (V.Vector Int) -> [Char]
convertVector2Str len vec = let vecStr = fmap (\x -> convertNum2Str x len) vec in
                            foldl (\x y -> x ++ y) "" vecStr

printMatrix :: Int -> (V.Vector (V.Vector Int)) -> IO ()
printMatrix len vec = mapM_ putStrLn (fmap (convertVector2Str len) vec)

start :: IO ()
start = printMatrix 5 (V.generate 4 (\n -> V.replicate 4 n))
