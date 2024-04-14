module Main (main) where

import Lib
import Second
import Tree
import Data.List
import System.Environment
import GHC.Base (VecElem(Int16ElemRep))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["csp", iterations, treeName, optimizations, propagationInterval] -> do
            let iterationValue = read iterations :: Int
                propagationIntervalValue = read propagationInterval :: Int
                optimizationList = read optimizations :: [Optimization]
                tree = case treeName of
                    "testTree2" -> testTree2
                    "testTree3" -> testTree3
                    _ -> error "Unknown tree name"

            (Lib.FinalResult ftree falloc performance) <- Lib.run iterationValue tree optimizationList propagationIntervalValue
            putStr ("Final tree: " ++ show ftree ++ "\n---\n" ++ "Allocations: " ++ show falloc ++ "\n---\n" ++ "Evaluations: " ++ show performance ++ "\n---\n" ++ "Average evaluations: " ++ show (fromIntegral (sum performance) / fromIntegral (length performance)))
        ["rbdt", iterations, treeName, greedyMode] -> do
            let iterationValue = read iterations :: Int
                greedyModeValue = read greedyMode :: EvaluationMode
                tree = case treeName of
                    "testTree2" -> testTree2
                    "testTree3" -> testTree3
                    _ -> error "Unknown tree name"
            (Second.FinalResult itree dtree example classes performance) <- Second.run iterationValue tree RBDT greedyModeValue
            putStr ("Final tree: " ++ show itree ++ "\n---\n" ++ "Decision tree: " ++ show dtree ++ "\n---\n" ++ "Allocations: " ++ show (zip3 example classes performance) ++ "\n---\n" ++ "Average evaluations: " ++ show (fromIntegral (sum performance) / fromIntegral (length performance)))
        ["gini", iterations, treeName, greedyMode, minSamplesSplit, maxDepth] -> do
            let iterationValue = read iterations :: Int
                greedyModeValue = read greedyMode :: EvaluationMode
                tree = case treeName of
                    "testTree2" -> testTree2
                    "testTree3" -> testTree3
                    _ -> error "Unknown tree name"
                minSamplesSplitValue = read minSamplesSplit :: Int
                maxDepthValue = read maxDepth :: Int
            (Second.FinalResult itree dtree example classes performance) <- Second.run iterationValue tree (GiniManipulation minSamplesSplitValue maxDepthValue) greedyModeValue
            putStr ("Final tree: " ++ show itree ++ "\n---\n" ++ "Decision tree: " ++ show dtree ++ "\n---\n" ++ "Allocations: " ++ show (zip3 example classes performance) ++ "\n---\n" ++ "Average evaluations: " ++ show (fromIntegral (sum performance) / fromIntegral (length performance)))
        ["debug"] -> do
            --putStr (show ((addCapacity (transformInventoryTree testTree3) . buildTree . flip transformToInput (GiniManipulation 3 10) . transformInventoryTree) testTree3))
            --putStr $ show $ getCapacity (transformInventoryTree testTree3)
            --putStr $ show $ getInformationGain (mustBeNonEmpty [Example 0 0 0 NoMaterial ["A"]]) [Example 0 0 0 NoMaterial ["A"]] []
            --putStr $ show $ 0 < getInformationGain (mustBeNonEmpty
                --[(Example 0 0 0 NoMaterial ["A"]), (Example 0 0 0 NoMaterial ["A"]), (Example 0 0 0 NoMaterial ["B"]), 
                 --Example 0 0 0 NoMaterial ["B"], (Example 0 0 0 NoMaterial ["C"]), (Example 0 0 0 NoMaterial ["C"]), (Example 0 0 0 NoMaterial ["D"]),
                 --(Example 0 0 0 NoMaterial ["D"]), (Example 0 0 0 NoMaterial ["E"]), (Example 0 0 0 NoMaterial ["E"]), (Example 0 0 0 NoMaterial ["F"]), (Example 0 0 0 NoMaterial ["F"])])
                --([(Example 0 0 0 NoMaterial ["A"]), (Example 0 0 0 NoMaterial ["B"]), (Example 0 0 0 NoMaterial ["C"]),
                 --(Example 0 0 0 NoMaterial ["D"]), (Example 0 0 0 NoMaterial ["E"]), (Example 0 0 0 NoMaterial ["F"])])
                --([(Example 0 0 0 NoMaterial ["A"]), (Example 0 0 0 NoMaterial ["B"]), (Example 0 0 0 NoMaterial ["C"]),
                 --(Example 0 0 0 NoMaterial ["D"]), (Example 0 0 0 NoMaterial ["E"]), (Example 0 0 0 NoMaterial ["F"])])
            putStr $ show $ transformToInput (transformInventoryTree testTree3) (GiniManipulation 3 10)
            --putStr $ show $ toRuleSet $ Rule ["RowA"] (\a b c d -> a < 5 && b < 6 && c < 7 && d `elem` [Wood, NoMaterial])
            --putStr $ show $ (\x -> (find (\h -> x h 0 0 NoMaterial)) [0..]) (\a b c d -> a < 5 && b < 6 && c < 7 && d `elem` [Wood, NoMaterial])
        _ -> putStrLn "Usage: programName <iterations> <treeName> <optimizations> <propagationInterval>"