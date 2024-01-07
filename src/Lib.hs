{-# LANGUAGE LambdaCase #-}
module Lib where

import System.Random ( randomRIO )
import Control.Monad.IO.Class ( liftIO )
import Data.List
import Debug.Trace

data Size = Size Int Int Int deriving Show
data Material = Wood | Metal | Glass deriving (Eq, Show)

data Constraint = SizeConstraint Int Int Int Int (Int -> Int -> Int -> Bool) | CapacityConstraint Int Int Int (Int -> Bool) | MaterialConstraint Int [Material] (Material -> Bool)

instance Show Constraint where
  show (SizeConstraint c x y z _) =
    "SizeConstraint: " ++ show c ++ " " ++ show x ++ " " ++ show y ++ " " ++ show z
  show (CapacityConstraint c _ x _) =
    "CapacityConstraint: " ++ show c ++ " " ++ show x
  show (MaterialConstraint c x _) =
    "MaterialConstraint: " ++ show c ++ " " ++ show x

data Item = Item Material Size deriving Show
data InventoryTree = Leaf String Order [Constraint] Int Int [Item] | Node String Order [Constraint] Int Int [InventoryTree] deriving Show

data Optimization = CapacitySave | ConstraintPropagation | ConstraintOrdering | ValueOrderingHeuristics deriving (Eq, Show, Read)
data Result = Result InventoryTree Bool (Maybe TreePosition) Int deriving Show
data FinalResult = FinalResult InventoryTree [TreePosition] [Int] deriving Show
data EvaluationResult = EvaluationResult Bool Int deriving Show
type Probability = Double
type TreePosition = [Int]
type Order = Int

testTree :: InventoryTree
testTree =
    Node "Root" 0 [SizeConstraint 0 6 7 8 (\x y z -> x <= 6 && y <= 7 && z <= 8),
          CapacityConstraint 0 4 6 (<= 6),
          MaterialConstraint 0 [Wood, Metal] (`elem` [Wood, Metal])] 1 1
        [
            Leaf "Leaf" 0 [CapacityConstraint 0 3 4 (<= 4)] 1 1 [Item Wood (Size 5 5 6)]
        ]

testTree2 :: InventoryTree
testTree2 =
    Node "Root" 0 [] 1 1
        [
            Node "RowA" 0 [MaterialConstraint 0 [Wood, Metal] (`elem` [Wood, Metal])] 1 1
                [
                    Node "ColA" 0 [SizeConstraint 0 7 9 1 (\x y z -> x <= 7 && y <= 9 && z <= 1)] 1 1
                        [
                            Node "SecA" 0 [] 1 1
                                [
                                    Leaf "BinA" 0 [CapacityConstraint 0 8 9 (<= 9)] 1 1 [],
                                    Leaf "BinB" 1 [CapacityConstraint 0 6 10 (<= 10)] 1 1 []
                                ],
                            Node "SecB" 1 [] 1 1
                                [
                                    Leaf "BinC" 0 [] 1 1 [],
                                    Leaf "BinD" 1 [] 1 1 []
                                ]
                        ],
                    Leaf "BinE" 1 [MaterialConstraint 0 [Wood, Metal] (`elem` [Wood, Metal]),
                                   SizeConstraint 0 2 2 2 (\x y z -> x <= 2 && y <= 2 && z <= 2)] 1 1 []
                ],
            Node "RowB" 1 [] 1 1
                [
                    Leaf "BinF" 0 [MaterialConstraint 0 [Glass] (`elem` [Glass])] 1 1 [],
                    Leaf "BinG" 1 [MaterialConstraint 0 [Metal] (`elem` [Metal])] 1 1 []
                ],
            Node "RowC" 2 [] 1 1
                [
                    Node "ColB" 0 [CapacityConstraint 0 3 51 (<= 51)] 1 1
                        [
                            Node "SecC" 0 [SizeConstraint 0 10 3 1 (\x y z -> x <= 10 && y <= 3 && z <= 1)] 1 1
                                [
                                    Leaf "BinH" 0 [] 1 1 [],
                                    Leaf "BinI" 1 [] 1 1 []
                                ]
                        ],
                    Leaf "BinJ" 1 [SizeConstraint 0 10 3 1 (\x y z -> x <= 10 && y <= 3 && z <= 1)] 1 1 []
                ]
        ]

testTree3 :: InventoryTree
testTree3 = Node "Root" 0 [] 1 1
                [
                    Node "RowA" 0 [SizeConstraint 0 10 9 7 (\x y z -> x <= 10 && y <= 9 && z <= 7),
                                   MaterialConstraint 0 [Glass] (`elem` [Glass])] 1 1
                        [
                            Node "ColA" 0 [] 1 1
                                [
                                    Leaf "BinA" 0 [SizeConstraint 0 7 8 9 (\x y z -> x <= 7 && y <= 8 && z <= 9),
                                                   CapacityConstraint 0 0 5 (<= 5)] 1 1
                                                   [Item Glass (Size 4 4 4),Item Glass (Size 1 3 7),Item Glass (Size 1 5 6),
                                                    Item Glass (Size 7 8 1)]
                                ],
                            Node "ColB" 1 [SizeConstraint 0 5 4 6 (\x y z -> x <= 5 && y <= 4 && z <= 6)] 1 1
                                [
                                    Leaf "BinB" 0 [CapacityConstraint 0 0 6 (<= 6)] 1 1 [Item Glass (Size 5 3 1)],
                                    Leaf "BinC" 1 [CapacityConstraint 0 0 4 (<= 4)] 1 1 [Item Glass (Size 2 1 3)]
                                ],
                            Node "ColC" 2 [] 1 1
                                [
                                    Leaf "BinD" 0 [CapacityConstraint 0 0 10 (<= 10)] 1 1 [Item Glass (Size 10 3 5),Item Glass (Size 3 1 4),Item Glass (Size 7 9 2),
                                                                                         Item Glass (Size 3 3 6),Item Glass (Size 9 7 4),Item Glass (Size 8 9 2),
                                                                                         Item Glass (Size 5 8 5),Item Glass (Size 3 6 4)]
                                ]
                        ],
                    Node "RowB" 1 [SizeConstraint 0 8 10 9 (\x y z -> x <= 8 && y <= 10 && z <= 9),
                                   MaterialConstraint 0 [Metal] (`elem` [Metal])] 1 1
                        [
                            Node "ColD" 0 [] 1 1
                                [
                                    Leaf "BinE" 0 [CapacityConstraint 0 0 8 (<= 8)] 1 1 [Item Metal (Size 1 5 7),Item Metal (Size 1 6 1),Item Metal (Size 8 3 8),
                                                                                       Item Metal (Size 5 8 1),Item Metal (Size 7 9 7),Item Metal (Size 7 3 7)],
                                    Leaf "BinF" 1 [CapacityConstraint 0 0 9 (<= 9)] 1 1 [Item Metal (Size 2 10 1),Item Metal (Size 3 2 8),Item Metal (Size 5 10 8),
                                                                                        Item Metal (Size 6 8 6),Item Metal (Size 5 7 1),Item Metal (Size 6 10 1)],
                                    Leaf "BinG" 2 [SizeConstraint 0 5 4 3 (\x y z -> x <= 5 && y <= 4 && z <= 3),
                                                   CapacityConstraint 0 0 11 (<= 11)] 1 1 [Item Metal (Size 3 2 1)]
                                ]
                        ],
                    Node "RowC" 2 [SizeConstraint 0 5 4 6 (\x y z -> x <= 5 && y <= 4 && z <= 6),
                                   CapacityConstraint 0 0 50 (<= 50)] 1 1
                        [
                            Node "ColE" 0 [MaterialConstraint 0 [Wood] (`elem` [Wood])] 1 1
                                [
                                    Leaf "BinH" 0 [CapacityConstraint 0 0 12 (<= 12)] 1 1 []
                                ],
                            Node "ColF" 1 [SizeConstraint 0 5 5 4 (\x y z -> x <= 5 && y <= 5 && z <= 4),
                                           MaterialConstraint 0 [Metal] (`elem` [Metal])] 1 1
                                [
                                    Leaf "BinI" 0 [CapacityConstraint 0 0 15 (<= 15)] 1 1 [Item Metal (Size 5 2 4)]
                                ],
                            Node "ColG" 2 [SizeConstraint 0 4 4 4 (\x y z -> x <= 4 && y <= 4 && z <= 4),
                                           MaterialConstraint 0 [Glass, Wood] (`elem` [Glass, Wood])] 1 1
                                [
                                    Leaf "BinJ" 0 [CapacityConstraint 0 0 14 (<= 14)] 1 1 []
                                ]
                        ],
                    Node "RowD" 3 [SizeConstraint 0 8 10 9 (\x y z -> x <= 8 && y <= 10 && z <= 9),
                                   MaterialConstraint 0 [Wood, Glass] (`elem` [Wood, Glass])] 1 1
                        [
                            Node "ColH" 0 [SizeConstraint 0 6 5 4 (\x y z -> x <= 6 && y <= 5 && z <= 4),
                                           MaterialConstraint 0 [Wood] (`elem` [Wood])] 1 1
                                [
                                    Leaf "BinK" 0 [CapacityConstraint 0 0 20 (<= 20)] 1 1 [Item Wood (Size 4 3 4),Item Wood (Size 1 2 4),
                                                                                         Item Wood (Size 6 2 4),Item Wood (Size 4 1 1)]
                                ],
                            Node "ColI" 1 [SizeConstraint 0 8 9 9 (\x y z -> x <= 8 && y <= 9 && z <= 9),
                                           CapacityConstraint 0 0 20 (<= 20)] 1 1
                                [
                                    Leaf "BinL" 0 [CapacityConstraint 0 0 12 (<= 12)] 1 1 [Item Glass (Size 1 1 7),Item Glass (Size 7 2 6),Item Wood (Size 3 7 4),
                                                                                         Item Glass (Size 2 5 5),Item Glass (Size 7 4 7),Item Glass (Size 2 3 7),
                                                                                         Item Glass (Size 1 8 9),Item Wood (Size 7 8 5),Item Wood (Size 5 4 9)],
                                    Leaf "BinM" 1 [] 1 1 [Item Wood (Size 4 3 9),Item Wood (Size 2 9 2),Item Glass (Size 5 1 8),
                                                          Item Wood (Size 5 9 2),Item Glass (Size 4 4 7),Item Glass (Size 6 3 6)]
                                ]
                        ],
                    Node "RowE" 4 [SizeConstraint 0 10 9 7 (\x y z -> x <= 10 && y <= 9 && z <= 7),
                                   MaterialConstraint 0 [Metal] (`elem` [Metal])] 1 1
                        [
                            Node "ColJ" 0 [SizeConstraint 0 9 9 7 (\x y z -> x <= 9 && y <= 9 && z <= 7),
                                           CapacityConstraint 0 0 30 (<= 30)] 1 1
                                [
                                    Leaf "BinN" 0 [SizeConstraint 0 5 4 3 (\x y z -> x <= 5 && y <= 4 && z <= 3)] 1 1 [Item Metal (Size 1 1 3)],
                                    Leaf "BinO" 1 [CapacityConstraint 0 0 10 (<= 10)] 1 1 [Item Metal (Size 9 9 7),Item Metal (Size 8 4 4),Item Metal (Size 9 4 1),
                                                                                         Item Metal (Size 9 6 2),Item Metal (Size 7 1 4),Item Metal (Size 6 9 5),
                                                                                         Item Metal (Size 4 5 2)]
                                ]
                        ]
                ]

traverseTree :: InventoryTree -> Item -> [Optimization] -> IO Result
traverseTree tree@(Node j ord c g b n) i o = let eval@(EvaluationResult res trys) = trace ("evaluateConstraints in traverseTree: " ++ j ++ show (evaluateConstraints tree i o c)) $ evaluateConstraints tree i o c in
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
                                    else return $ Result (Node j ord (trace ("!!!reorder!!!" ++ j ++ show c ++ show eval ++ show (reOrderConstraints o c eval)) $ reOrderConstraints o c eval) g b n) False Nothing trys
traverseTree tree@(Leaf j ord c g b is) i o = let eval@(EvaluationResult res trys) = evaluateConstraints tree i o c in
                                    if res
                                    then return $ Result (Leaf j ord c g b (i:is)) True (Just [ord]) trys
                                    else return $ Result (Leaf j ord (trace ("!!!reorder!!!" ++ j ++ show c ++ show eval ++ show (reOrderConstraints o c eval)) $ reOrderConstraints o c eval) g b is) False Nothing   trys

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
run' 1 tree o _ _  = do
                    randomItem <- getRandomItem
                    Result tree' _ pos score <- traverseTree tree randomItem o
                    treeRem' <- removeRandomItem tree'
                    return $ case pos of
                        Just p  -> FinalResult treeRem' [p] [score]
                        Nothing -> FinalResult tree'  []  []
run' n tree o i 0  = do
                    randomItem <- getRandomItem
                    Result tree' _ pos score <- traverseTree (trace ("constraint propagation: " ++ show tree ++ "    /    " ++ show (applyConstraintPropagation tree o)) $ applyConstraintPropagation tree o) randomItem o
                    treeRem' <- removeRandomItem tree'
                    FinalResult tree'' ps score' <- run' (pred n) treeRem' o i i
                    return $ case pos of
                        Just p  -> FinalResult tree'' (p:ps) (score : score')
                        Nothing -> FinalResult tree'' ps     score'
run' n tree o i ci = do
                    randomItem <- getRandomItem
                    Result tree' _ pos score <- traverseTree tree randomItem o
                    treeRem' <- removeRandomItem tree'
                    FinalResult tree'' ps score' <- run' (pred n) treeRem' o i (pred ci)
                    return $ case pos of
                        Just p  -> FinalResult tree'' (p:ps) (score : score')
                        Nothing -> FinalResult tree'' ps     score'

removeRandomItem :: InventoryTree -> IO InventoryTree
removeRandomItem t = removeRandomItem' (getItemCount t) t
removeRandomItem' :: Int -> InventoryTree -> IO InventoryTree
removeRandomItem' _ t@(Leaf _ _ _ _ _ []) = return t
removeRandomItem' x (Leaf a b c d e (i:is)) = do
            num <- randomRIO (1, x)
            (Leaf a' b' c' d' e' is') <- removeRandomItem' x (Leaf a b c d e is)
            return $ if num == 1 then Leaf a b c d e is else Leaf a' b' c' d' e' (i:is')
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
applyConstraintPropagation' (Node j x c y z n)  = let (curr, under) = reassemble n $ pushDown $ pushUp $ trace ("before pushUp" ++ show ([] :: [Constraint], c, map (\case
                                                        (Node _ _ c' _ _ _) -> c'
                                                        (Leaf _ _ c' _ _ _) -> c') n)) ([], c, map (\case
                                                        (Node _ _ c' _ _ _) -> c'
                                                        (Leaf _ _ c' _ _ _) -> c') n) in
                                                        Node j x curr y z (map applyConstraintPropagation' under)

filterConstraints :: [[Constraint]] -> [[Constraint]]
filterConstraints [] = []
filterConstraints constraintsLists =
  let commonConstraints = foldr1 (intersectBy sameType) constraintsLists
  in trace ("after filterConstraints" ++ show (map (filter (`sameTypes` commonConstraints)) constraintsLists)) $ map (filter (`sameTypes` commonConstraints)) constraintsLists

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
merge c@((SizeConstraint {}):_) = foldr (\(SizeConstraint _ x y z ci) (SizeConstraint _ x' y' z' ci') -> SizeConstraint 0 (max x x') (max y y') (max z z') (\x' y' z' -> x' >= 0 && y' >= 0 && z' >= 0))
                    (SizeConstraint 0 0 0 0 (\x'' y'' z'' -> x'' >= 0 && y'' >= 0 && z'' >= 0)) c
merge c@((CapacityConstraint {}):_) = foldr (\(CapacityConstraint _ s c ci) (CapacityConstraint _ s' c' ci') -> CapacityConstraint 0 (s + s') (c + c') (<= (c + c')))
                    (CapacityConstraint 0 0 0 (<= 0)) c
merge c@((MaterialConstraint {}):_)  = foldr (\(MaterialConstraint _ ms mi) (MaterialConstraint _ ms' mi') -> MaterialConstraint 0 (union ms ms') (`elem` (union ms ms')))
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
                            mergedConstraints = map merge $ trace ("before groupConstraints: " ++ show propConstraints ++ " after groupConstraints: " ++ show (groupConstraints propConstraints)) $ groupConstraints propConstraints in
                                trace ("after pushUp: " ++ show (mergedConstraints,map merge (groupConstraints [mergedConstraints, up]),down)) (mergedConstraints,map merge (groupConstraints [mergedConstraints, up]),down)

pushDown :: ([Constraint],[Constraint],[[Constraint]]) -> ([Constraint],[Constraint],[[Constraint]])
pushDown pl@(nc, up, down) = let propConstraints = filterWasPushedUp pl $ filterNotContainsDown down $ removeFrom nc $ filterSecondHalf up in
                            trace ("after pushDown: " ++ show (propConstraints,removeFrom propConstraints up,down)) (propConstraints,removeFrom propConstraints up,down)

reassemble :: [InventoryTree] -> ([Constraint],[Constraint],[[Constraint]]) -> ([Constraint],[InventoryTree])
reassemble t (_,y,z) = (y,zipWith (curry (\case
                                    (Node x'''' x _ x' x'' x''',c') -> Node x'''' x c' x' x''  x'''
                                    (Leaf x'''' x _ x' x'' x''',c') -> Leaf x'''' x c' x' x''  x''')) t z)