{-# LANGUAGE LambdaCase #-}
module Lib where

import System.Random ( randomRIO )
import Control.Monad.IO.Class ( liftIO )
import Data.List
import Debug.Trace
import Tree

data Optimization = CapacitySave | ConstraintPropagation | ConstraintOrdering | ValueOrderingHeuristics deriving (Eq, Show, Read)
data Result = Result InventoryTree Bool (Maybe TreePosition) Int deriving Show
data FinalResult = FinalResult InventoryTree [TreePosition] [Int] deriving Show
data EvaluationResult = EvaluationResult Bool Int deriving Show
type Probability = Double
type TreePosition = [Int]

traverseTree :: InventoryTree -> Item -> [Optimization] -> IO Result
traverseTree tree@(Node j ord c g b n) i o = let eval@(EvaluationResult res trys) = evaluateConstraints tree i o c in
                                    if res
                                    then
                                        do on         <- applyValueOrdering n o
                                           retResults <- mapM (\on' -> liftIO $ traverseTree on' i o) on
                                           let results = applyWeightingAdaptation $ getAllUntilSuccess retResults
                                           let result  = last results
                                           case result of
                                               Result (Node {}) _ (Just pos) _ ->
                                                return $ Result
                                                (Node j ord c g b (reOrder (mergeTraversed on (map (\(Result t' _ _ _) -> t') results))))
                                                True
                                                (Just (ord:pos))
                                                (sum (map (\(Result _ _ _ o') -> o') results) + trys)
                                               Result (Leaf {}) _ (Just pos) _ ->
                                                return $ Result
                                                (Node j ord c g b (reOrder (mergeTraversed on (map (\(Result t' _ _ _) -> t') results))))
                                                True
                                                (Just (ord:pos))
                                                (sum (map (\(Result _ _ _ o') -> o') results) + trys)
                                               Result (Node {}) _ Nothing _ ->
                                                return $ Result
                                                (Node j ord c g b (reOrder (mergeTraversed on (map (\(Result t' _ _ _) -> t') results))))
                                                True
                                                Nothing
                                                (sum (map (\(Result _ _ _ o') -> o') results) + trys)
                                               Result (Leaf {}) _ Nothing _ ->
                                                return $ Result
                                                (Node j ord c g b (reOrder (mergeTraversed on (map (\(Result t' _ _ _) -> t') results))))
                                                True
                                                Nothing
                                                (sum (map (\(Result _ _ _ o') -> o') results) + trys)
                                    else return $ Result (Node j ord (reOrderConstraints o c eval) g b n) False Nothing trys
traverseTree tree@(Leaf j ord c g b is) i o = let eval@(EvaluationResult res trys) = evaluateConstraints tree i o c in
                                    if res
                                    then return $ Result (Leaf j ord c g b (i:is)) True (Just [ord]) trys
                                    else return $ Result (Leaf j ord (reOrderConstraints o c eval) g b is) False Nothing trys

getAllUntilSuccess :: [Result] -> [Result]
getAllUntilSuccess (res@(Result _ _ Nothing _):xs) = res : getAllUntilSuccess xs
getAllUntilSuccess (res@(Result _ _ (Just _) _):_) = [res]
getAllUntilSuccess [] = []

mergeTraversed :: [InventoryTree] -> [InventoryTree] -> [InventoryTree]
mergeTraversed (o@(Node _ ord _ _ _ _):os) new = case find (\case
                                                            (Node _ ord' _ _ _ _) -> ord == ord'
                                                            (Leaf _ ord' _ _ _ _) -> ord == ord') new of
                                                Just n  -> n : mergeTraversed os new
                                                Nothing -> o : mergeTraversed os new
mergeTraversed (o@(Leaf _ ord _ _ _ _):os) new = case find (\case
                                                            (Node _ ord' _ _ _ _) -> ord == ord'
                                                            (Leaf _ ord' _ _ _ _) -> ord == ord') new of
                                                Just n  -> n : mergeTraversed os new
                                                Nothing -> o : mergeTraversed os new
mergeTraversed [] _ = []

evaluateConstraints :: InventoryTree -> Item -> [Optimization] -> [Constraint] -> EvaluationResult
evaluateConstraints _ _ _ [] = EvaluationResult True 0
evaluateConstraints t i o c  = case findIndex (not . evaluateConstraint t i o) c of
                                Just x  -> EvaluationResult False $ succ x
                                Nothing -> EvaluationResult True $ length c

evaluateConstraint :: InventoryTree -> Item -> [Optimization] -> Constraint -> Bool
evaluateConstraint tree (Item m (Size x y z)) o c
    | CapacitySave `elem` o = case c of SizeConstraint _ _ _ _ sc -> sc x y z
                                        CapacityConstraint _ cs _ cc -> cc cs
                                        MaterialConstraint _ _ mc -> mc m
    | otherwise           = case c of SizeConstraint _ _ _ _ sc -> sc x y z
                                      CapacityConstraint _ _ _ cc -> cc $ succ $ getItemCount tree
                                      MaterialConstraint _ _ mc -> mc m

getItemCount :: InventoryTree -> Int
getItemCount (Node _ _ _ _ _ (n:ns)) = getItemCount n + sum (map getItemCount ns)
getItemCount (Node _ _ _ _ _ []) = error "Tree has empty node"
getItemCount (Leaf _ _ _ _ _ i) = length i

reOrderConstraints :: [Optimization] -> [Constraint] -> EvaluationResult -> [Constraint]
reOrderConstraints o c (EvaluationResult res trys)
    | ConstraintPropagation `elem` o || ConstraintOrdering `elem` o = if not res then sortBy (flip getBlockOrder) $ incrementCountAt (pred trys) c else c
    | otherwise = c
    where
        getBlockOrder :: Constraint -> Constraint -> Ordering
        getBlockOrder x y = compare (getBlock x) (getBlock y)
            where
                getBlock :: Constraint -> Int
                getBlock (SizeConstraint c' _ _ _ _)   = c'
                getBlock (CapacityConstraint c' _ _ _) = c'
                getBlock (MaterialConstraint c' _ _)   = c'
        incrementCountAt :: Int -> [Constraint] -> [Constraint]
        incrementCountAt k constraints
            | k >= 0 && k < length constraints =
                take k constraints ++ updatedElement : drop (succ k) constraints
            | otherwise = constraints
            where
                updatedElement = increment (constraints !! k)
                increment :: Constraint -> Constraint
                increment (SizeConstraint a b c' d e) =
                    SizeConstraint (succ a) b c' d e
                increment (CapacityConstraint a b c' d) =
                    CapacityConstraint (succ a) b c' d
                increment (MaterialConstraint a materials b) =
                    MaterialConstraint (succ a) materials b

swapToFirst :: Int -> [a] -> [a]
swapToFirst x xs
  | x < 0 || x >= length xs = xs
  | otherwise = let (before, after) = splitAt x xs
                    (atX, rest) = splitAt 1 after
                in atX ++ before ++ rest

applyWeightingAdaptation :: [Result] -> [Result]
applyWeightingAdaptation [] = []
applyWeightingAdaptation ((Result (Node x y z g b w) e i j):rs) = if e
                                                                       then Result (Node x y z (succ g) b w) e i j : applyWeightingAdaptation rs
                                                                       else Result (Node x y z g (succ b) w) e i j : applyWeightingAdaptation rs
applyWeightingAdaptation ((Result (Leaf x y z g b w) e i j):rs) = if e
                                                                       then Result (Leaf x y z (succ g) b w) e i j : applyWeightingAdaptation rs
                                                                       else Result (Leaf x y z g (succ b) w) e i j : applyWeightingAdaptation rs

applyValueOrdering :: [InventoryTree] -> [Optimization] -> IO [InventoryTree]
applyValueOrdering t o = if ValueOrderingHeuristics `elem` o
                         then randomWeightList t
                         else return t

randomWeightList :: [InventoryTree] -> IO [InventoryTree]
randomWeightList [] = return []
randomWeightList t = do
    ct <- randomWeightedChoice t
    let remaining = filter (\case
                                Node _ ord _ _ _ _ -> ord /= getOrder ct
                                Leaf _ ord _ _ _ _ -> ord /= getOrder ct) t
    rt <- randomWeightList remaining
    return $ ct : rt
  where
    getOrder (Node _ ord _ _ _ _) = ord
    getOrder (Leaf _ ord _ _ _ _) = ord

randomWeightedChoice :: [InventoryTree] -> IO InventoryTree
randomWeightedChoice tree = do
    let totalWeight = sum $ map (\case
                                    t@(Node {}) -> (calculateFullProbability t)
                                    t@(Leaf {}) -> (calculateFullProbability t)) tree
    randomValue <- randomRIO (0, totalWeight)
    return $ selectElement randomValue tree

selectElement :: Double -> [InventoryTree] -> InventoryTree
selectElement _ [] = error "Empty list"
selectElement randomValue (tree@(Node {}):rest)
    | randomValue <= calculateFullProbability tree = tree
    | otherwise = selectElement (randomValue - calculateFullProbability tree) rest
selectElement randomValue (tree@(Leaf {}):rest)
    | randomValue <= calculateFullProbability tree = tree
    | otherwise = selectElement (randomValue - calculateFullProbability tree) rest

calculateFullProbability :: InventoryTree -> Double
calculateFullProbability (Leaf _ _ _ g n _) = (fromIntegral g / (fromIntegral g + fromIntegral n)) / (fromIntegral n / (fromIntegral g + fromIntegral n))
calculateFullProbability (Node _ _ _ g n s) = (fromIntegral g / (fromIntegral g + fromIntegral n)) / (fromIntegral n / (fromIntegral g + fromIntegral n)) * sum (map calculateFullProbability s)

reOrder :: [InventoryTree] -> [InventoryTree]
reOrder = sortBy compareNodes
  where
    compareNodes (Node _ o0 _ _ _ _) (Node _ o1 _ _ _ _) = compare o0 o1
    compareNodes (Node _ o0 _ _ _ _) (Leaf _ o1 _ _ _ _) = compare o0 o1
    compareNodes (Leaf _ o0 _ _ _ _) (Node _ o1 _ _ _ _) = compare o0 o1
    compareNodes (Leaf _ o0 _ _ _ _) (Leaf _ o1 _ _ _ _) = compare o0 o1

------------------------------------------------------

run :: Int -> InventoryTree -> [Optimization] -> Int -> IO FinalResult
run n tree o i = run' n tree o i i
run' :: Int -> InventoryTree -> [Optimization] -> Int -> Int -> IO FinalResult
run' 1 tree o i ci  = do
                    randomItem <- getRandomItem
                    Result tree' _ pos score <- traverseTree tree randomItem o
                    treeRem' <- removeRandomItem pos tree'
                    case pos of
                        Just p  -> return $ FinalResult treeRem' [p] [score]
                        Nothing -> run' 1 tree o i ci
run' n tree o i 0  = do
                    randomItem <- getRandomItem
                    Result tree' _ pos score <- traverseTree (applyConstraintPropagation tree o) randomItem o
                    treeRem' <- removeRandomItem pos tree'
                    FinalResult tree'' ps score' <- run' (if pos == Nothing then n else pred n) treeRem' o i i
                    return $ case pos of
                        Just (_:p)  -> FinalResult tree'' (p:ps) (score : score')
                        Just [] -> error "Not possible"
                        Nothing -> FinalResult tree'' ps     score'
run' n tree o i ci = do
                    randomItem <- getRandomItem
                    Result tree' _ pos score <- traverseTree tree randomItem o
                    treeRem' <- removeRandomItem pos tree'
                    FinalResult tree'' ps score' <- run' (if pos == Nothing then n else pred n) treeRem' o i (pred ci)
                    return $ case pos of
                        Just (_:p)  -> FinalResult tree'' (p:ps) (score : score')
                        Just [] -> error "Not possible"
                        Nothing -> FinalResult tree'' ps     score'

removeRandomItem :: Maybe TreePosition -> InventoryTree -> IO InventoryTree
removeRandomItem Nothing t = return t
removeRandomItem _       t = removeRandomItem' (getItemCount t) t
removeRandomItem' :: Int -> InventoryTree -> IO InventoryTree
removeRandomItem' _ t@(Leaf _ _ _ _ _ []) = return t
removeRandomItem' x (Leaf a b c d e (i:is)) = do
            num <- randomRIO (0, x)
            (Leaf a' b' c' d' e' is') <- removeRandomItem' x (Leaf a b c d e is)
            return $ if num == 0 then Leaf a' b' c' d' e' is' else Leaf a' b' c' d' e' (i:is')
removeRandomItem' x (Node name order constraints width height children) = do
    updatedChildren <- mapM (removeRandomItem' x) children
    return $ Node name order constraints width height updatedChildren

getRandomMaterial :: IO Material
getRandomMaterial = do
  let materials = [Wood, Metal, Glass]
  randomIndex <- randomRIO (0, length materials - 1)
  return $ materials !! randomIndex

getRandomSize :: IO Size
getRandomSize = do
  x <- randomRIO (1, 10)
  y <- randomRIO (1, 10)
  z <- randomRIO (1, 10)
  return $ Size x y z

getRandomItem :: IO Item
getRandomItem = do
  randomMaterial <- getRandomMaterial
  Item randomMaterial <$> getRandomSize

applyConstraintPropagation :: InventoryTree -> [Optimization] -> InventoryTree
applyConstraintPropagation t o
    | ConstraintPropagation `elem` o && ConstraintOrdering `elem` o = applyConstraintPropagation' t
    | otherwise = t

applyConstraintPropagation' :: InventoryTree -> InventoryTree
applyConstraintPropagation' tree@(Leaf {})  = tree
applyConstraintPropagation' (Node j x c y z n)  = let (curr, under) = reassemble n $ pushDown $ pushUp ([], c, map (\case
                                                        (Node _ _ c' _ _ _) -> c'
                                                        (Leaf _ _ c' _ _ _) -> c') n) in
                                                        Node j x curr y z (map applyConstraintPropagation' under)

filterConstraints :: [[Constraint]] -> [[Constraint]]
filterConstraints [] = []
filterConstraints constraintsLists =
  let commonConstraints = foldr1 (intersectBy sameType) constraintsLists
  in map (filter (`sameTypes` commonConstraints)) constraintsLists

filterFirstHalf :: [Constraint] -> [Constraint]
filterFirstHalf [] = []
filterFirstHalf (x@(SizeConstraint c _ _ _ _):_) = [x | c > 0]
filterFirstHalf (x@(CapacityConstraint c _ _ _):_) = [x | c > 0]
filterFirstHalf (x@(MaterialConstraint c _ _):_) = [x | c > 0]

filterSecondHalf :: [Constraint] -> [Constraint]
filterSecondHalf [] = []
filterSecondHalf (_:xs) = xs

filterNotContainsDown :: [[Constraint]] -> [Constraint] -> [Constraint]
filterNotContainsDown x y =  removeFrom y (head (filterConstraints x))

filterWasPushedUp :: ([Constraint],[Constraint],[[Constraint]]) -> [Constraint] -> [Constraint]
filterWasPushedUp (_, _, down) curr = let (_, nup, _) = pushUp ([], [], down) in
                                        intersectBy sameConstraint curr nup

removeFrom :: [Constraint] -> [Constraint] -> [Constraint]
removeFrom xs constraints
  = foldl
      (\ constraints' x -> filter (\ c -> not (sameType c x)) constraints')
      constraints xs

sameType :: Constraint -> Constraint -> Bool
sameType (SizeConstraint {}) (SizeConstraint {}) = True
sameType (CapacityConstraint {}) (CapacityConstraint {}) = True
sameType (MaterialConstraint {}) (MaterialConstraint {}) = True
sameType _ _ = False

sameConstraint :: Constraint -> Constraint -> Bool
sameConstraint (SizeConstraint _ x y z _) (SizeConstraint _ x' y' z' _) = x == x' && y == y' && z == z'
sameConstraint (CapacityConstraint _ x y _) (CapacityConstraint _ x' y' _) = x == x' && y == y'
sameConstraint (MaterialConstraint _ x _) (MaterialConstraint _ x' _) = x == x'
sameConstraint _ _ = False

sameTypes :: Constraint -> [Constraint] -> Bool
sameTypes _ [] = False
sameTypes c (x:xs) = sameType c x || sameTypes c xs

merge :: [Constraint] -> Constraint
merge c@((SizeConstraint {}):_) = foldr (\(SizeConstraint _ x y z ci) (SizeConstraint _ x' y' z' ci') -> SizeConstraint 0 (max x x') (max y y') (max z z') (\x''' y''' z''' -> x''' <= max x x' && y''' <= max y y' && z''' <= max z z'))
                    (SizeConstraint 0 0 0 0 (\x'' y'' z'' -> x'' <= 0 && y'' <= 0 && z'' <= 0)) c
merge c@((CapacityConstraint {}):_) = foldr (\(CapacityConstraint _ s c ci) (CapacityConstraint _ s' c' ci') -> CapacityConstraint 0 (s + s') (c + c') (<= (c + c')))
                    (CapacityConstraint 0 0 0 (<= 0)) c
merge c@((MaterialConstraint {}):_)  = foldr (\(MaterialConstraint _ ms mi) (MaterialConstraint _ ms' mi') -> MaterialConstraint 0 (ms `union` ms') (`elem` union ms ms'))
                    (MaterialConstraint 0 [] (`elem` [])) c
merge [] = error "Cannot merge empty list"

groupConstraints :: [[Constraint]] -> [[Constraint]]
groupConstraints [] = []
groupConstraints x = groupBy sameType $ sortBy compareConstraints $ concat x

compareConstraints :: Constraint -> Constraint -> Ordering
compareConstraints (SizeConstraint {}) (MaterialConstraint {}) = LT
compareConstraints (MaterialConstraint {}) (CapacityConstraint {}) = LT
compareConstraints _ _ = EQ

pushUp :: ([Constraint],[Constraint],[[Constraint]]) -> ([Constraint],[Constraint],[[Constraint]])
pushUp (nc, up, down) = let propConstraints = map (removeFrom nc) $ filterConstraints $ map filterFirstHalf down
                            mergedConstraints = map merge $ groupConstraints propConstraints in
                                (mergedConstraints,map merge (groupConstraints [mergedConstraints, up]),down)

pushDown :: ([Constraint],[Constraint],[[Constraint]]) -> ([Constraint],[Constraint],[[Constraint]])
pushDown pl@(nc, up, down) = let propConstraints = filterWasPushedUp pl $ filterNotContainsDown down $ removeFrom nc $ filterSecondHalf up in
                            (propConstraints,removeFrom propConstraints up,down)

reassemble :: [InventoryTree] -> ([Constraint],[Constraint],[[Constraint]]) -> ([Constraint],[InventoryTree])
reassemble t (_,y,z) = (y,zipWith (curry (\case
                                    (Node x'''' x _ x' x'' x''',c') -> Node x'''' x c' x' x''  x'''
                                    (Leaf x'''' x _ x' x'' x''',c') -> Leaf x'''' x c' x' x''  x''')) t z)