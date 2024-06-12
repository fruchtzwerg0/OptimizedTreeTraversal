module Main (main) where

import Lib
import Second
import Tree
import Data.List
import System.Environment
import Second ((~!~))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["csp", iterations, treeName, optimizations, propagationInterval] -> do
            let iterationValue = read iterations :: Int
                propagationIntervalValue = read propagationInterval :: Int
                optimizationList = read optimizations :: [Optimization]
                tree = case treeName of
                    "testTree1" -> testTree1
                    _ -> error "Unknown tree name"

            (Lib.FinalResult ftree falloc performance) <- Lib.run iterationValue tree optimizationList propagationIntervalValue
            putStr ("Final tree: " ++ show ftree ++ "\n---\n" ++ "Allocations: " ++ show falloc ++ "\n---\n" ++ "Evaluations: " ++ show performance ++ "\n---\n" ++ "Average evaluations: " ++ show (fromIntegral (sum performance) / fromIntegral (length performance)))
        ["rbdt", iterations, treeName, greedyMode] -> do
            let iterationValue = read iterations :: Int
                greedyModeValue = read greedyMode :: EvaluationMode
                tree = case treeName of
                    "testTree1" -> testTree1
                    _ -> error "Unknown tree name"
            (Second.FinalResult itree dtree example classes performance) <- Second.run tree RBDT iterationValue greedyModeValue
            putStr ("Final tree: " ++ show itree ++ "\n---\n" ++ "Decision tree: " ++ show dtree ++ "\n---\n" ++ "Allocations: " ++ show performance ++ "\n---\n" ++ "Average evaluations: " ++ show (fromIntegral (sum performance) / fromIntegral (length performance)))
        ["gini", iterations, treeName, greedyMode] -> do
            let iterationValue = read iterations :: Int
                greedyModeValue = read greedyMode :: EvaluationMode
                tree = case treeName of
                    "testTree1" -> testTree1
                    _ -> error "Unknown tree name"
                minSamplesSplitValue = 3--read minSamplesSplit :: Int
                maxDepthValue = 10--read maxDepth :: Int
            (Second.FinalResult itree dtree example classes performance) <- Second.run tree (GiniManipulation minSamplesSplitValue maxDepthValue) iterationValue greedyModeValue
            putStr ("Final tree: " ++ show itree ++ "\n---\n" ++ "Decision tree: " ++ show dtree ++ "\n---\n" ++ "Allocations: " ++ show performance ++ "\n---\n" ++ "Average evaluations: " ++ show (fromIntegral (sum performance) / fromIntegral (length performance)))
        ["debug"] -> do
            --(zip3 example classes performance)
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
            --putStr $ show $ buildTree $ transformToInput (transformInventoryTree testTree3) RBDT
            --putStr $ show $ ((head $ map snd $ toNonEmpty $ transformToRules $ transformInventoryTree testTree3))
            --putStr $ show $ ((\(Rule _ f) -> f 1 1 1 Glass) $ (~!~) ((map snd $ toNonEmpty $ transformToRules $ transformInventoryTree testTree3) !! 1))
            --putStr $ show $ ((map snd $ toNonEmpty $ transformToRules $ transformInventoryTree testTree3) !! 1)
            --putStr $ show $ (head $ map snd $ toNonEmpty $ transformToRules $ transformInventoryTree testTree3)
            --putStr $ show $ ((~&~) ((~!~) $ (map snd $ toNonEmpty $ transformToRules $ transformInventoryTree testTree3) !! 1) (head $ map snd $ toNonEmpty $ transformToRules $ transformInventoryTree testTree3))
            putStr $ show $ (makeDisjoint $ map snd $ toNonEmpty $ transformToRules $ transformInventoryTree testTree1)
            --putStr $ show $ Rule ["RowA"] (\x _ _ _ -> x == 3 || x == 4 || x == 0)
            --putStr $ show $ (\x -> (find (\h -> x h 0 0 NoMaterial)) [0..]) (\a b c d -> a < 5 && b < 6 && c < 7 && d `elem` [Wood, NoMaterial])
        _ -> putStrLn "Usage:\nprogramName csp  <iterations> <treeName (e.g. testTree1)> <optimizations (e.g. [ConstraintPropagation, ConstraintOrdering, ValueOrderingHeuristics])> <propagationInterval>\nprogramName rbdt <iterations> <treeName (e.g. testTree1)> Greedy|NonGreedy\nprogramName gini <iterations> <treeName (e.g. testTree1)> Greedy|NonGreedy"