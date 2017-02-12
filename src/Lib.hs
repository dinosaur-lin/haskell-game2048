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

convertMatrix2Str :: Int -> (V.Vector (V.Vector Int)) -> [Char]
convertMatrix2Str len mat = let mstr = fmap (\x -> convertVector2Str len x) mat in
                              foldl (\x y -> x ++ "\n" ++ y) "" mstr

printMatrix :: Int -> (V.Vector (V.Vector Int)) -> IO ()
printMatrix len mat = putStrLn (convertMatrix2Str len mat)

start :: IO ()
start = printMatrix 5 (V.generate 4 (\n -> V.replicate 4 n))
