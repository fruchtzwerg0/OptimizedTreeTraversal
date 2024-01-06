module Main (main) where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [iterations, treeName, optimizations, propagationInterval] -> do
            let iterationValue = read iterations :: Int
                propagationIntervalValue = read propagationInterval :: Int
                optimizationList = read optimizations :: [Optimization]
                tree = case treeName of
                    "testTree2" -> testTree2
                    "testTree3" -> testTree3
                    _ -> error "Unknown tree name"

            result@(FinalResult _ _ performance) <- run iterationValue tree optimizationList propagationIntervalValue
            print (result, fromIntegral (sum performance) / fromIntegral (length performance))

        _ -> putStrLn "Usage: programName <iterations> <treeName> <optimizations> <propagationInterval>"