{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Second where

import System.Random ( randomRIO )
import Tree
import Data.List
import qualified Data.List.NonEmpty as N
import Lib ( getRandomItem, FinalResult (FinalResult), evaluateConstraints, EvaluationResult (EvaluationResult), removeRandomItem, getItemCount, reassemble )
import Debug.Trace

data Approach = RBDT | GiniManipulation Int Int
data EvaluationMode = Greedy | NonGreedy deriving Read
data DecisionTreeB = LeafB   (N.NonEmpty LeafContent) | NodeB (Int -> Int -> Int -> Material -> Bool) DecisionTreeB DecisionTreeB
data DecisionTreeNB = LeafNB (N.NonEmpty LeafContent) | NodeNB (N.NonEmpty ([Int], Int -> Int -> Int -> Material -> Bool)) (N.NonEmpty DecisionTreeNB)
data Example = Example Int Int Int Material Class deriving (Eq, Show)
data Rule = Rule Class (Int -> Int -> Int -> Material -> Bool)

instance Show Rule where
    show r@(Rule c _) = "Rule " ++ show c ++ " ~{~" ++ (show . head . toRuleSet) r ++ ", " ++ (show . last . toRuleSet) r ++ "~}~ "

instance Show DecisionTreeB where
    show (NodeB f l r) = "NodeB (" ++ showDecision2 f ++ " " ++ show l ++ " " ++ show r ++ ")"
    show (LeafB c) = "LeafB (" ++ (show . toNonEmpty) c ++ ")"

instance Show DecisionTreeNB where
    show (NodeNB f s) = "NodeNB (" ++ show (toNonEmpty $ N.map (showDecision2 . snd) f) ++ " "  ++ (show . toNonEmpty) s ++ ")"
    show (LeafNB c) = "LeafNB (" ++ (show . toNonEmpty) c ++ ")"

instance Eq DecisionTreeNB where
    (NodeNB f0 s0) == (NodeNB f1 s1) = N.map fst f0 == N.map fst f1 && s0 == s1
    (LeafNB l0) == (LeafNB l1) = l0 == l1
    _ == _ = False

instance Ord DecisionTreeNB where
    compare (NodeNB f0 s0) (NodeNB f1 s1) =
        case compare (length f0) (length f1) of
          EQ -> case compare (length s0) (length s1) of
                  EQ -> case compareMap ((map fst . toNonEmpty) f0) ((map fst . toNonEmpty) f1) of
                          EQ -> compareMap (toNonEmpty s0) (toNonEmpty s1)
                          z  -> z
                  z -> z
          z -> z
          where
            compareMap :: Ord a => [a] -> [a] -> Ordering
            compareMap x@(_:xs) y@(_:ys) =
                case compare x y of
                  EQ -> compareMap xs ys
                  z  -> z
            compareMap [] [] = EQ
            compareMap _ _ = error "must be same length"
    compare (LeafNB l0) (LeafNB l1) = compare l0 l1
    compare (LeafNB _) (NodeNB _ _) = LT
    compare (NodeNB _ _) (LeafNB _) = GT

instance Eq Rule where
    r0@(Rule x _) == r1@(Rule y _) = x == y && sameRuleSet (==) r0 r1

data Input = Examples Int Int (N.NonEmpty Example) | Rules (N.NonEmpty Rule) deriving Show

data Result = Result DecisionTreeB Example Class Int deriving Show
data FinalResult = FinalResult InventoryTree DecisionTreeB [Example] [Class] [Int]
type Class = [String]

data LeafContent = LeafContent Class Int Int deriving (Show, Eq, Ord)

type FiniteDomainFlag = Bool

mapShowDecision :: [Int -> Int -> Int -> Material -> Bool] -> String
mapShowDecision = foldl (\x y -> x ++ ", " ++ showDecision2 y) ""

showDecision :: (Int -> Int -> Int -> Material -> Bool) -> String
showDecision f = case (find (\x -> not $ f x 0 0 (toMaterial 0)) [0..20]) of
                    Just y -> "Length <= " ++ show (pred y)
                    Nothing -> case (find (\x -> not $ f 0 x 0 (toMaterial 0)) [0..20]) of
                                    Just y -> "Width <= " ++ show (pred y)
                                    Nothing -> case (find (\x -> not $ f 0 0 x (toMaterial 0)) [0..20]) of
                                                    Just y -> "Thickness <= " ++ show (pred y)
                                                    Nothing -> case (find (\x -> not $ f 0 0 0 (toMaterial x)) [0..3]) of
                                                                    Just y -> "Material <= " ++ show (toMaterial (pred y))
                                                                    Nothing -> error "not possible"

showDecision2 :: (Int -> Int -> Int -> Material -> Bool) -> String
showDecision2 f = case filter (\x -> f (-1) (-1) (-1) (toMaterial x)) [1..3] of
                    [] -> case (filter (\x -> f (-1) x (-1) (toMaterial 0)) [1..20]) of
                                    [] -> case (filter (\x -> f (-1) (-1) x (toMaterial 0)) [1..20]) of
                                                    [] -> case (filter (\x -> f x (-1) (-1) (toMaterial 0)) [1..20]) of
                                                                    [] -> error "not possible"
                                                                    y -> "Length == " ++ show y
                                                    y -> "Thickness == " ++ show y
                                    y -> "Width == " ++ show y
                    y -> "Material == " ++ show (map toMaterial y)

transformInventoryTree :: InventoryTree -> InventoryTreeSecond
transformInventoryTree (Node a b c d e (f:fs)) = NodeS a b c d e $ do
    fND <- f N.:| fs
    (return . transformInventoryTree) fND
transformInventoryTree (Leaf a b c d e f) = LeafS a b c d e f
transformInventoryTree (Node _ _ _ _ _ []) = error "Node has no items"

updateCapacities :: InventoryTree -> InventoryTree
updateCapacities n@(Node a b c d e f)
    | ((<) 0 . length) cc = Node a b ((flip (:) cnc . updateCapacity (head cc) . getItemCount) n) d e $ do
        fND <- f
        (return . updateCapacities) fND
    | otherwise = Node a b c d e $ do
        fND <- f
        (return . updateCapacities) fND
    where
        cc = (filter isCapacity . mconcat . groupConstraints) c
        cnc = (filter (not . isCapacity) . mconcat . groupConstraints) c
        updateCapacity (CapacityConstraint a _ c d) x = CapacityConstraint a x c d
updateCapacities n@(Leaf a b c d e f)
    | ((<) 0 . length) cc = Leaf a b ((flip (:) cnc . updateCapacity (head cc) . getItemCount) n) d e f
    | otherwise = n
    where
        cc = (filter isCapacity . mconcat . groupConstraints) c
        cnc = (filter (not . isCapacity) . mconcat . groupConstraints) c
        updateCapacity (CapacityConstraint a _ c d) x = CapacityConstraint a x c d

run :: InventoryTree -> Approach -> Int -> EvaluationMode -> IO Second.FinalResult
run tree a = run' ((buildTree . flip transformToInput a . transformInventoryTree) tree) tree a

run' :: DecisionTreeB -> InventoryTree -> Approach -> Int -> EvaluationMode -> IO Second.FinalResult
run' dtree tree a 1 e = do
                    randomItem <- getRandomItem
                    let res = predict (transformInventoryTree tree) dtree e (toExample randomItem)
                    case res of
                        Just (Result dtree' x c o) -> do
                            let tree' = inputToTree tree c randomItem
                            case tree' of
                                Just t -> do
                                    treeRem <- Second.removeRandomItem t
                                    return $ Second.FinalResult (updateCapacities treeRem) (addCapacity ((transformInventoryTree . updateCapacities) treeRem) dtree') [x] [c] [o]
                                Nothing -> trace (show $ "Doesn't fit and unnoticed: " ++ show randomItem ++ show c) $ run' dtree tree a 1 e
                        Nothing -> run' dtree tree a 1 e
run' dtree tree a n e = do
                    randomItem <- getRandomItem
                    let res = predict (transformInventoryTree tree) dtree e (toExample randomItem)
                    case res of
                        Just (Result dtree' x c o)  -> do
                            let tree' = inputToTree tree c randomItem
                            case tree' of
                                Just t -> do
                                    treeRem' <- Second.removeRandomItem t
                                    Second.FinalResult tree'' dtree'' x' c' o' <- run' dtree' (updateCapacities treeRem') a (pred n) e
                                    return $ Second.FinalResult tree'' dtree'' (x : x') (c : c') (o : o')
                                Nothing -> trace (show $ "Doesn't fit but unnoticed: " ++ show randomItem ++ " " ++ show c) $ do
                                    Second.FinalResult tree'' dtree'' x' c' o' <- run' dtree tree a n e
                                    return $ Second.FinalResult (updateCapacities tree'') (addCapacity ((transformInventoryTree . updateCapacities) tree'') dtree'') x' c' o'
                        Nothing -> do
                            Second.FinalResult tree'' dtree'' x' c' o' <- run' dtree tree a n e
                            return $ Second.FinalResult (updateCapacities tree'') (addCapacity ((transformInventoryTree . updateCapacities) tree'') dtree'') x' c' o'

removeRandomItem :: InventoryTree -> IO InventoryTree
removeRandomItem t = removeRandomItem' (getItemCount t) t
removeRandomItem' :: Int -> InventoryTree -> IO InventoryTree
removeRandomItem' _ t@(Leaf _ _ _ _ _ []) = return t
removeRandomItem' x (Leaf a b c d e (i:is)) = do
            num <- randomRIO (0, x)
            (Leaf a' b' c' d' e' is') <- removeRandomItem' x (Leaf a b c d e is)
            return $ if num == 0 then Leaf a' b' c' d' e' is' else Leaf a' b' c' d' e' (i:is')
removeRandomItem' x (Node name order constraints width height children) = do
    updatedChildren <- mapM (removeRandomItem' x) children
    return $ Node name order constraints width height updatedChildren

inputToTree :: InventoryTree -> Class -> Item -> Maybe InventoryTree
inputToTree t c i = if getItemCount t == (getItemCount . inputToTree' t c) i then Nothing else Just (inputToTree' t c i)
inputToTree' :: InventoryTree -> Class -> Item -> InventoryTree
inputToTree' t@(Node w x cs y z s) c i = Node w x cs y z $ case evaluateConstraints t i [] cs of
    EvaluationResult True _ -> do
        sND <- s
        [inputToTree' sND c i]
    _ -> s
inputToTree' t@(Leaf b w x y z is) [c] i = if b == c then Leaf b w x y z (i:is) else t
inputToTree' _ _ _ = error "class must be singleton"

toExample :: Item -> Example
toExample (Item m (Size x y z)) = Example x y z m []

predict :: InventoryTreeSecond -> DecisionTreeB -> EvaluationMode -> Example -> Maybe Result
predict t d e x = (makePredictions False e x . addCapacity t) d

buildTree :: Input -> DecisionTreeB
buildTree (Examples minss maxd x) = buildBTree x minss maxd
buildTree (Rules x) = (convertToBinaryTree . buildNBTree) x

convertToBinaryTree :: DecisionTreeNB -> DecisionTreeB
convertToBinaryTree (NodeNB (_ N.:| []) (y0 N.:| [])) = convertToBinaryTree y0
convertToBinaryTree (NodeNB (x0 N.:| [_]) (y0 N.:| [y1])) = NodeB (snd x0) (convertToBinaryTree y0) (convertToBinaryTree y1)
convertToBinaryTree (NodeNB (x N.:| xs) (y N.:| ys)) = NodeB (snd x) (convertToBinaryTree y) (convertToBinaryTree (NodeNB (mustBeNonEmpty xs) (mustBeNonEmpty ys)))
convertToBinaryTree (LeafNB x) = LeafB x

makePredictions :: Bool -> EvaluationMode -> Example -> DecisionTreeB -> Maybe Result
makePredictions o e@NonGreedy x@(Example h w t m _) d@(NodeB f l r) = incrementResult d $ if f h w t m then makePredictions o e x l else makePredictions o e x r
makePredictions o NonGreedy ex x@(LeafB ((LeafContent c cap ccap) N.:| _)) = if cap == ccap then Nothing else (Just . Result x ex c) (boolToInt o)
makePredictions o e@Greedy x@(Example h w t m _) d@(NodeB f l r) = incrementResult d $
    if f h w t m
    then if (not . isMaterialDecision) f || (not . materialDecisionsInTree) d || (isNothing . predictGreedily e x) r
         then makePredictions o e x l
         else predictGreedily e x r
    else makePredictions o e x r
makePredictions o Greedy ex x@(LeafB ((LeafContent c cap ccap) N.:| _)) = if cap == ccap then Nothing else (Just . Result x ex c) (boolToInt o)

predictGreedily :: EvaluationMode -> Example -> DecisionTreeB -> Maybe Result
predictGreedily e x d@(NodeB _ l r) = if isNothing (predictGreedily e x r) then (incrementResult d . makePredictions True e x) l else predictGreedily e x r
predictGreedily _ ex d@(LeafB ((LeafContent c cap ccap) N.:| _)) = if cap == ccap then Nothing else (Just . Result d ex c) 1

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True  = 1

incrementResult :: DecisionTreeB -> Maybe Result -> Maybe Result
incrementResult t (Just (Result _ x c o)) = (Just . Result t x c . succ) o
incrementResult _ Nothing = Nothing

materialDecisionsInTree :: DecisionTreeB -> Bool
materialDecisionsInTree (NodeB f l r) = or $ isMaterialDecision f : do
    sND <- [l, r]
    [materialDecisionsInTree sND]
materialDecisionsInTree _ = False

isMaterialDecision :: (Int -> Int -> Int -> Material -> Bool) -> Bool
isMaterialDecision f = all (f 0 0 0) allMaterials

addCapacity :: InventoryTreeSecond -> DecisionTreeB -> DecisionTreeB
addCapacity it dt = (orderCapacity . putCapacity dt . getCapacity) it

orderCapacity :: DecisionTreeB -> DecisionTreeB
orderCapacity (NodeB x l r) = NodeB x (orderCapacity l) (orderCapacity r)
orderCapacity (LeafB xs) = LeafB (N.sortBy (\(LeafContent _ ccap0 cap0) (LeafContent _ ccap1 cap1) -> compare (cap1 - ccap1) (cap0 - ccap0)) xs)

getCapacity :: InventoryTreeSecond -> N.NonEmpty (Class, Constraint)
getCapacity (NodeS n _ c _ _ s) = do
    sND <- s
    case zip [[n]] ((filter isCapacity . mconcat . groupConstraints) c) of
        [] -> getCapacity sND
        x  -> N.map (tupleMap (<>) (head x)) (getCapacity sND)
    where
        tupleMap :: (a -> a -> a) -> (b, a) -> (b, a) -> (b, a)
        tupleMap op (_, y1) (x2, y2) = (x2, op y1 y2)
getCapacity (LeafS n _ c _ _ _) =
    case zip [[n]] ((filter isCapacity . mconcat . groupConstraints) c) of
        [] -> ([n], NoConstraint) N.:| []
        (x0:_) -> x0 N.:| []

isCapacity :: Constraint -> Bool
isCapacity (CapacityConstraint {}) = True
isCapacity _ = False

putCapacity :: DecisionTreeB -> N.NonEmpty (Class, Constraint) -> DecisionTreeB
putCapacity (NodeB x l r) cc = NodeB x (putCapacity l cc) (putCapacity r cc)
putCapacity (LeafB xs) cc =
    LeafB $ N.map (\(LeafContent c _ _) ->
        let i = (determine . nElemIndex c) (N.map fst cc) in
            LeafContent c (getCurrentCapacity $ N.map snd cc N.!! i) (getCapacity $ N.map snd cc N.!! i)) xs
    where
        getCurrentCapacity :: Constraint -> Int
        getCurrentCapacity (CapacityConstraint _ c _ _) = c
        getCapacity :: Constraint -> Int
        getCapacity (CapacityConstraint _ _ c _) = c

buildBTree :: N.NonEmpty Example -> Int -> Int -> DecisionTreeB
buildBTree x minss maxd = let (l, r, _, f1, _, i) = getBestSplit x
                          in if length x >= minss && maxd /= 0 && i > 0
                             then NodeB f1 (buildBTree l minss (pred maxd)) (buildBTree r minss (pred maxd))
                             else (LeafB . N.nubBy (\(LeafContent y _ _) (LeafContent z _ _) -> y == z) . N.map (\(Example _ _ _ _ c) -> LeafContent c 0 0)) x

showw (x, y, _, _, _, _) = show x ++ "/                                \\" ++ show y

getBestSplit :: N.NonEmpty Example -> (N.NonEmpty Example, N.NonEmpty Example, Example -> Int, Int -> Int -> Int -> Material -> Bool, Int, Double)
getBestSplit x =
    (foldr (\y@(_, _, _, _, _, yv) z@(_, _, _, _, _, zv) -> if yv >= zv then y else z)
    (Example 0 0 0 NoMaterial [] N.:| [], Example 0 0 0 NoMaterial [] N.:| [], const 0, \_ _ _ _ -> False, 0, 0) .
    (concatMap . getSplits) x) [(getLength, setLengthDecision), (getWidth, setWidthDecision), (getThickness, setThicknessDecision), (getMaterial, setMaterialDecision)]

getSplits :: N.NonEmpty Example -> (Example -> Int, Int -> (Int -> Int -> Int -> Material -> Bool)) ->
    [(N.NonEmpty Example, N.NonEmpty Example, Example -> Int, Int -> Int -> Int -> Material -> Bool, Int, Double)]
getSplits x (f0, f1) =
    foldr ((\(x0, x1, x2, x3, x4, x5) yt -> case (x0, x1) of
        (y0:yn0, y1:yn1) -> (y0 N.:| yn0, y1 N.:| yn1, x2, x3, x4, x5):yt
        _ -> yt) . (\y -> (first y, second y, f0, f1 y, y, getInformationGain x (first y) (second y))))
        [] ((N.nub . N.map f0) x)
    where
        first  = fst . splitBy x f0
        second = snd . splitBy x f0

splitBy :: N.NonEmpty Example -> (Example -> Int) -> Int -> ([Example], [Example])
splitBy x f t = (N.filter (\y -> f y <= t) x, N.filter (\y -> f y > t) x)

getInformationGain :: N.NonEmpty Example -> [Example] -> [Example] -> Double
getInformationGain a@(a0 N.:| an) l r = giniIndex (a0:an) - (wl * giniIndex l + wr * giniIndex r)
    where
        wl = (fromIntegral . length) l / (fromIntegral . length) a
        wr = (fromIntegral . length) r / (fromIntegral . length) a

giniIndex :: [Example] -> Double
giniIndex x = 1 - foldr (\y z -> cls y ** 2 + z) 0 (exampleClasses x)
    where
        cls :: Class -> Double
        cls y = (fromIntegral . length . filter (\(Example _ _ _ _ c) -> c == y)) x / (fromIntegral . length) x

exampleClasses :: [Example] -> [Class]
exampleClasses = map (\(Example _ _ _ _ c) -> c) . nubBy eqC

buildNBTree :: N.NonEmpty Rule -> DecisionTreeNB
buildNBTree r = buildNBTree' r [(attributeLength, \v x _ _ _ -> x == v || x == 0, 0),
                                (attributeWidth,  \v _ x _ _ -> x == v || x == 0, 1),
                                (attributeThickness, \v _ _ x _ -> x == v || x == 0, 2),
                                (attributeMaterial, \v _ _ _ x -> toInt x == v || x == NoMaterial, 3)]

buildNBTree' :: N.NonEmpty Rule -> [([Rule] -> [Int], Int -> Int -> Int -> Int -> Material -> Bool, Int)] -> DecisionTreeNB
buildNBTree' r@(r0 N.:| rs) as@(_:_) = mergeEqual $ NodeNB (N.map (\(x, y, _) -> ([x], y x)) currPred) $ do
    (num, currPredND, ind) <- currPred
    return $ buildNBTree' (mustBeNonEmpty (filter (any (\(Example l w d m _) -> currPredND num l w d m) . toRuleSet) (toNonEmpty r))) (deleteFirst (\(_, _, x) -> x == ind) as)
    where a = getFittest (r0:rs) as
          currPred = (mustBeNonEmpty . sortBy (\x y -> compare (fst3 x) (fst3 y)) . map (, snd3 a, trd3 a)) ((nub . fst3 a . toNonEmpty) r)
          mergeEqual :: DecisionTreeNB -> DecisionTreeNB
          mergeEqual (NodeNB f0 s0) =
            let res = (mustBeNonEmpty . map (\x -> ((N.head . N.map fst) x, foldr1 mergeNode (N.map snd x))) .
                    N.groupBy (\(x, _) (y, _) -> x == y) . N.sortBy (\(x, _) (y, _) -> compare x y) . N.zip s0) f0 in
            NodeNB (N.map snd res) (N.map fst res)
          mergeNode :: ([Int], Int -> Int -> Int -> Material -> Bool) ->
                       ([Int], Int -> Int -> Int -> Material -> Bool) ->
                       ([Int], Int -> Int -> Int -> Material -> Bool)
          mergeNode (num0, x) (num1, y) = (num0 ++ num1, \l w t m -> x l w t m || y l w t m)
buildNBTree' r [] =
    (LeafNB . N.nubBy (\(LeafContent x _ _) (LeafContent y _ _) -> x == y) . nConcatMap1 (\(Rule c _) -> N.map (\ic -> LeafContent [ic] 0 0) (mustBeNonEmpty c))) r

determine :: Maybe a -> a
determine (Just y) = y
determine Nothing = error "cannot happen because four attributes present"

transformToInput :: InventoryTreeSecond -> Approach -> Input
transformToInput t RBDT
    | (not . all fst . transformToRules) t = error "ruleset is infinite"
    | otherwise = (Rules . mustBeNonEmpty . makeDisjoint . toNonEmpty) ((N.map snd . transformToRules) t)
transformToInput t (GiniManipulation minss maxd)
    | (not . all fst . transformToRules) t = error "ruleset is infinite"
    | otherwise = (Examples minss maxd . mustBeNonEmpty . transformToExamples . toNonEmpty) ((N.map snd . transformToRules) t)

transformToExamples :: [Rule] -> [Example]
transformToExamples rs = (nub . concatMap (\r@(Rule c _) -> do
    l <- map getMaxLengthOfRule rs
    w <- map getMaxWidthOfRule rs
    t <- map getMaxThicknessOfRule rs
    map (\m -> Example (min l (getMaxLengthOfRule r)) (min w (getMaxWidthOfRule r)) (min t (getMaxThicknessOfRule r)) (toMaterial m) c)
     (getAllMaterialsOfRule r))) rs

canWrapAroundCuboid :: Rule -> Rule -> Bool
canWrapAroundCuboid r0 r1 = getMaxLengthOfRule r0    >= getMaxLengthOfRule r1 &&
                            getMaxWidthOfRule  r0    >= getMaxWidthOfRule  r1 &&
                            getMaxThicknessOfRule r0 >= getMaxThicknessOfRule r1

transformToRules :: InventoryTreeSecond -> N.NonEmpty (FiniteDomainFlag, Rule)
transformToRules (NodeS _ _ c _ _ s) = do
    sND <- s
    frND <- transformToRules sND
    return $ (foldr
      (\ cn fr
          -> ((-~&~-) fr . constraintToRuleWithoutClass (fst fr)) cn)
      (constraintToRuleWithoutClass False mempty)
      . mconcat . groupConstraints) c -~&~- frND
transformToRules (LeafS b _ c _ _ _) =
    (return . foldr (\cn fr -> ((-~&~-) fr .
     constraintToRule [b] (fst fr)) cn) (constraintToRule [b] False mempty) . mconcat . groupConstraints) c

constraintToRule :: Class -> FiniteDomainFlag -> Constraint -> (FiniteDomainFlag, Rule)
constraintToRule c f cn =
    ((fst . constraintToRuleWithoutClass f) cn, (snd . constraintToRuleWithoutClass f) cn ~&~ Rule c (\_ _ _ _ -> True))

constraintToRuleWithoutClass :: FiniteDomainFlag -> Constraint -> (FiniteDomainFlag, Rule)
constraintToRuleWithoutClass _ (SizeConstraint _ _ _ _ cn) = (True, Rule [] (\l w t _ -> cn l w t || (l == 0 && w == 0 && t == 0)))
constraintToRuleWithoutClass f (MaterialConstraint _ _ cn) = (f, Rule [] (\_ _ _ m -> cn m || m == NoMaterial))
constraintToRuleWithoutClass f _ = (f, Rule [] (\_ _ _ _ -> True))

makeDisjoint :: [Rule] -> [Rule]
makeDisjoint =
    concatMap (makeSizeDisjoint) . groupBy (\x y -> eqM ((head . toRuleSet) x) ((head . toRuleSet) y)) .
    sortBy (\a b -> compare ((getMaterial . head . toRuleSet) a) ((getMaterial . head . toRuleSet) b)) .
    splitMaterialRules . map (foldr (~&~) (Rule [] (\_ _ _ _ -> True))) . groupBy (sameRuleSet eqWOC) . sortBy orderRuleSet

makeNonSubsetSizeRules :: [Rule] -> [Rule]
makeNonSubsetSizeRules x = (nubBy (sameRuleSet (==)) . concatMap (\y -> foldr (\a b -> if sameRuleSet (==) a y && (not . canWrapAroundCuboid a) y then b
    else Rule [] (extractMaterialsOutOfRule a) ~&~ makeNonSubsetSizeRule [(getMaxLengthOfRule, extractLengthOutOfRule),
                                                                          (getMaxWidthOfRule, extractWidthOutOfRule),
                                                                          (getMaxThicknessOfRule, extractThicknessOutOfRule)] a y:b) [] x)) x

makeNonSubsetSizeRule :: [(Rule -> Int, Rule -> (Int -> Int -> Int -> Material -> Bool))] -> Rule -> Rule -> Rule
makeNonSubsetSizeRule ((f, r):s) x@(Rule cx _) y@(Rule cy _) =
    Rule ((nub . (++) cx) cy) (if f x > f y then r y else r x) ~&~ makeNonSubsetSizeRule s x y
makeNonSubsetSizeRule [] _ _ = Rule [] (\_ _ _ _ -> True)

makeSizeDisjoint :: [Rule] -> [Rule]
makeSizeDisjoint x = map (makeRuleSizeDisjoint x
    [(\(Example l _ _ _ _) -> Rule [] (\l0 _ _ _ -> l0 == l || l0 == 0), \(Example l _ _ _ _) -> Rule [] (\l0 _ _ _ -> l0 > l || l0 == 0), getLength, getMaxLength),
     (\(Example _ w _ _ _) -> Rule [] (\_ w0 _ _ -> w0 == w || w0 == 0), \(Example _ w _ _ _) -> Rule [] (\_ w0 _ _ -> w0 > w || w0 == 0), getWidth, getMaxWidth),
     (\(Example _ _ t _ _) -> Rule [] (\_ _ t0 _ -> t0 == t || t0 == 0), \(Example _ _ t _ _) -> Rule [] (\_ _ t0 _ -> t0 > t || t0 == 0), getThickness, getMaxThickness)]) x

makeRuleSizeDisjoint :: [Rule] -> [(Example -> Rule, Example -> Rule, Example -> Int, Example -> Example -> [Example])] -> Rule -> Rule
makeRuleSizeDisjoint r ((r0,r1,r2,f):fs) x =
    let xMaxAndBelow =
         ((last . toRuleSet) x : filter (\ex -> ((==) 2 . length . f ex) ((last . toRuleSet) x))
         (map (last . toRuleSet) (delete x r)),
         find (\ex -> r2 ex < r2 ((last . toRuleSet) x)) $ sortBy (\a b -> r2 b `compare` r2 a) (map (last . toRuleSet) (delete x r))) in
    if (length . fst) xMaxAndBelow > 1
    then (makeRuleSizeDisjoint r fs . (~&~) x . r0 . head . fst) xMaxAndBelow
    else case snd xMaxAndBelow of
        Just c -> ((~&~) (Rule ((nub . concatMap (\(Rule cl _) -> cl) . filter (`canWrapAroundCuboid` x)) r) (\_ _ _ _ -> True)) . (~&~) x . r1) c
        Nothing -> x ~&~ Rule ((nub . concatMap (\(Rule cl _) -> cl) . filter (`canWrapAroundCuboid` x)) r) (\_ _ _ _ -> True)
makeRuleSizeDisjoint _ [] _ = error "not possible"

splitMaterialRules :: [Rule] -> [Rule]
splitMaterialRules = nub . concatMap (\x -> (map (\((Example _ _ _ m c):_) -> x ~&~ Rule c (\_ _ _ m0 -> m0 == m || m0 == NoMaterial)) .
                     groupBy eqM . sortBy (\y z -> compare (getMaterial y) (getMaterial z))) (toRuleSet x))

sameRuleSet :: (Example -> Example -> Bool) -> Rule -> Rule -> Bool
sameRuleSet eq x y
    | (length . toRuleSet) x == (length . toRuleSet) y = all (uncurry eq) (zip (toRuleSet x) (toRuleSet y))
    | otherwise = False

orderRuleSet :: Rule -> Rule -> Ordering
orderRuleSet x y
    | (length . toRuleSet) x == (length . toRuleSet) y =
        case compare (getMaxLengthOfRule x) (getMaxLengthOfRule y) of
          EQ -> case compare (getMaxWidthOfRule x) (getMaxWidthOfRule y) of
                  EQ -> case compare (getMaxThicknessOfRule x) (getMaxThicknessOfRule y) of
                          EQ -> case compare ((length . getAllMaterialsOfRule) x) ((length . getAllMaterialsOfRule) y) of
                                  EQ -> compareMaterials (getAllMaterialsOfRule x) (getAllMaterialsOfRule y)
                                  z  -> z
                          z  -> z
                  z  -> z
          z  -> z
    | (length . toRuleSet) x < (length . toRuleSet) y = LT
    | otherwise = GT
          where
            compareMaterials :: [Int] -> [Int] -> Ordering
            compareMaterials x@(_:xs) y@(_:ys) =
                case compare x y of
                  EQ -> compareMaterials xs ys
                  z  -> z
            compareMaterials [] [] = EQ
            compareMaterials _ _ = error "must be same length"


toRuleSet :: Rule -> [Example]
toRuleSet (Rule c x) = do
    he <- [((determine . find (\h -> x h 0 0 NoMaterial)) [1..])..pred ((determine . find (\h -> (not . x h 0 0) NoMaterial)) [((determine . find (\h -> x h 0 0 NoMaterial)) [1..])..])]
    wi <- [((determine . find (\w -> x 0 w 0 NoMaterial)) [1..])..pred ((determine . find (\w -> (not . x 0 w 0) NoMaterial)) [((determine . find (\w -> x 0 w 0 NoMaterial)) [1..])..])]
    th <- [((determine . find (\t -> x 0 0 t NoMaterial)) [1..])..pred ((determine . find (\t -> (not . x 0 0 t) NoMaterial)) [((determine . find (\t -> x 0 0 t NoMaterial)) [1..])..])]
    ma <- filter (x 0 0 0) [Wood, Metal, Glass]
    [Example he wi th ma c]
    where
        determine :: Maybe a -> a
        determine (Just y) = y
        determine Nothing = error "cannot happen because ruleset not infinite"

getConstraints :: InventoryTreeSecond -> [Constraint]
getConstraints (NodeS _ _ c _ _ _) = c
getConstraints (LeafS _ _ c _ _ _) = c

groupConstraints :: [Constraint] -> [[Constraint]]
groupConstraints [] = []
groupConstraints x = groupBy sameType $ sortBy compareConstraints x

sameType :: Constraint -> Constraint -> Bool
sameType (SizeConstraint {}) (SizeConstraint {}) = True
sameType (CapacityConstraint {}) (CapacityConstraint {}) = True
sameType (MaterialConstraint {}) (MaterialConstraint {}) = True
sameType NoConstraint NoConstraint = True
sameType _ _ = False

compareConstraints :: Constraint -> Constraint -> Ordering
compareConstraints (SizeConstraint {}) (MaterialConstraint {}) = LT
compareConstraints (MaterialConstraint {}) (CapacityConstraint {}) = LT
compareConstraints (CapacityConstraint {}) (NoConstraint {}) = LT
compareConstraints _ _ = EQ

(<#>) :: Input -> Input -> Input
Examples y z f <#> Examples _ _ x = Examples y z (f <> x)
Rules f <#> Rules x = Rules (f <> x)
_ <#> _ = error "not possible"

(~&~) :: Rule -> Rule -> Rule
Rule c0 x ~&~ Rule c1 y = Rule ((nub . (++) c0) c1) (\l w t m -> x l w t m && y l w t m)

(~|~) :: Rule -> Rule -> Rule
Rule c0 x ~|~ Rule c1 y = Rule (nub (c0 `intersect` c1)) (\l w t m -> x l w t m || y l w t m)

(-~&~-) :: (FiniteDomainFlag, Rule) -> (FiniteDomainFlag, Rule) -> (FiniteDomainFlag, Rule)
(x0,y0) -~&~- (x1,y1) = (x0 || x1, y0 ~&~ y1)

(-~|~-) :: (FiniteDomainFlag, Rule) -> (FiniteDomainFlag, Rule) -> (FiniteDomainFlag, Rule)
(x0,y0) -~|~- (x1,y1) = (x0 || x1, y0 ~|~ y1)

eqWOC :: Example -> Example -> Bool
eqWOC (Example h0 w0 t0 m0 _) (Example h1 w1 t1 m1 _) = h0 == h1 && w0 == w1 && t0 == t1 && m0 == m1

eqWOM :: Example -> Example -> Bool
eqWOM (Example h0 w0 t0 _ c0) (Example h1 w1 t1 _ c1) = h0 == h1 && w0 == w1 && t0 == t1 && c0 == c1

eqWOMC :: Example -> Example -> Bool
eqWOMC (Example h0 w0 t0 _ _) (Example h1 w1 t1 _ _) = h0 == h1 && w0 == w1 && t0 == t1

eqM :: Example -> Example -> Bool
eqM (Example _ _ _ m0 _) (Example _ _ _ m1 _) = m0 == m1

eqC :: Example -> Example -> Bool
eqC (Example _ _ _ _ c0) (Example _ _ _ _ c1) = c0 == c1

getLength :: Example -> Int
getLength (Example x _ _ _ _) = x

getWidth :: Example -> Int
getWidth (Example _ x _ _ _) = x

getThickness :: Example -> Int
getThickness (Example _ _ x _ _) = x

getMaterial :: Example -> Int
getMaterial (Example _ _ _ x _) = toInt x

setLengthDecision :: Int -> (Int -> Int -> Int -> Material -> Bool)
setLengthDecision x y _ _ _ = y <= x

setWidthDecision :: Int -> (Int -> Int -> Int -> Material -> Bool)
setWidthDecision x _ y _ _ = y <= x

setThicknessDecision :: Int -> (Int -> Int -> Int -> Material -> Bool)
setThicknessDecision x _ _ y _ = y <= x

setMaterialDecision :: Int -> (Int -> Int -> Int -> Material -> Bool)
setMaterialDecision x _ _ _ y = toInt y <= x

getMaxLengthOfRule :: Rule -> Int
getMaxLengthOfRule = foldr (\(Example y _ _ _ _) z -> max y z) 0 . toRuleSet

getMaxWidthOfRule :: Rule -> Int
getMaxWidthOfRule = foldr (\(Example _ y _ _ _) z -> max y z) 0 . toRuleSet

getMaxThicknessOfRule :: Rule -> Int
getMaxThicknessOfRule = foldr (\(Example _ _ y _ _) z -> max y z) 0 . toRuleSet

getAllLengthsOfRule :: Rule -> [Int]
getAllLengthsOfRule = nub . map (\(Example x _ _ _ _) -> x) . toRuleSet

getAllWidthsOfRule :: Rule -> [Int]
getAllWidthsOfRule = nub . map (\(Example _ x _ _ _) -> x) . toRuleSet

getAllThicknessOfRule :: Rule -> [Int]
getAllThicknessOfRule = nub . map (\(Example _ _ x _ _) -> x) . toRuleSet

getAllMaterialsOfRule :: Rule -> [Int]
getAllMaterialsOfRule = nub . map (\(Example _ _ _ x _) -> toInt x) . toRuleSet

extractLengthOutOfRule :: Rule -> (Int -> Int -> Int -> Material -> Bool)
extractLengthOutOfRule r a _ _ _ = a `elem` getAllLengthsOfRule r

extractWidthOutOfRule :: Rule -> (Int -> Int -> Int -> Material -> Bool)
extractWidthOutOfRule r a _ _ _ = a `elem` getAllWidthsOfRule r

extractThicknessOutOfRule :: Rule -> (Int -> Int -> Int -> Material -> Bool)
extractThicknessOutOfRule r a _ _ _ = a `elem` getAllThicknessOfRule r

extractMaterialsOutOfRule :: Rule -> (Int -> Int -> Int -> Material -> Bool)
extractMaterialsOutOfRule r a _ _ _ = a `elem` getAllMaterialsOfRule r

toInt :: Material -> Int
toInt NoMaterial = 0
toInt Wood = 1
toInt Metal = 2
toInt Glass = 3
toInt _ = error "no fourth material"

toMaterial :: Int -> Material
toMaterial 0 = NoMaterial
toMaterial 1 = Wood
toMaterial 2 = Metal
toMaterial 3 = Glass
toMaterial _ = error "no fourth material"

getMaxLength :: Example -> Example -> [Example]
getMaxLength x@(Example x1 _ _ _ _) y@(Example y1 _ _ _ _)
    | x1 == y1 = [x,y]
    | x1 >  y1 = [x]
    | otherwise = [y]

getMaxWidth :: Example -> Example -> [Example]
getMaxWidth x@(Example _ x1 _ _ _) y@(Example _ y1 _ _ _)
    | x1 == y1 = [x,y]
    | x1 >  y1 = [x]
    | otherwise = [y]

getMaxThickness :: Example -> Example -> [Example]
getMaxThickness x@(Example _ _ x1 _ _) y@(Example _ _ y1 _ _)
    | x1 == y1 = [x,y]
    | x1 >  y1 = [x]
    | otherwise = [y]

getMaxAttribute :: Ord a => (([Rule] -> [Int]) -> a) ->
    ([Rule] -> [Int], Int -> Int -> Int -> Int -> Material -> Bool, Int) ->
    [([Rule] -> [Int], Int -> Int -> Int -> Int -> Material -> Bool, Int)] ->
    [([Rule] -> [Int], Int -> Int -> Int -> Int -> Material -> Bool, Int)]
getMaxAttribute f x y
    | (f . fst3) x == (f . fst3 . head) y = x:y
    | (f . fst3) x >  (f . fst3 . head) y = [x]
    | otherwise = y

findMax :: (Example -> Example -> [Example]) -> [Example] -> [Example]
findMax x = foldr (\y z -> x y (head z)) [Example 0 0 0 NoMaterial []]

attributeLength :: [Rule] -> [Int]
attributeLength = concatMap getAllLengthsOfRule
attributeWidth :: [Rule] -> [Int]
attributeWidth = concatMap getAllWidthsOfRule
attributeThickness :: [Rule] -> [Int]
attributeThickness = concatMap getAllThicknessOfRule
attributeMaterial :: [Rule] -> [Int]
attributeMaterial = concatMap getAllMaterialsOfRule

getFittest :: [Rule] -> [([Rule] -> [Int], Int -> Int -> Int -> Int -> Material -> Bool, Int)] ->
    ([Rule] -> [Int], Int -> Int -> Int -> Int -> Material -> Bool, Int)
getFittest r a = getFittest' r [attributeEffectiveness r numberOfClasses, attributeAutonomy r (map fst3 a), minimumValueDistribution r] a

getFittest' :: Ord a => [Rule] -> [([Rule] -> [Int]) -> a] ->
    [([Rule] -> [Int], Int -> Int -> Int -> Int -> Material -> Bool, Int)] ->
    ([Rule] -> [Int], Int -> Int -> Int -> Int -> Material -> Bool, Int)
getFittest' r (f:fs) a =
    if 1 == length m
    then head m
    else getFittest' r fs m
    where
        m = foldr (getMaxAttribute f) [head a] (tail a)
getFittest' _ [] (a:_) = a
getFittest' _ [] [] = error "not possible"

--m
numberOfClasses :: [Rule] -> Int
numberOfClasses = length . nubBy (\(Rule c0 _) (Rule c1 _) -> c0 == c1)

classes :: [Rule] -> [Class]
classes = map (\(Rule c _) -> c) . nubBy (\(Rule c0 _) (Rule c1 _) -> c0 == c1)

--R
rules :: Class -> [Rule] -> [Rule]
rules c = filter (\(Rule c0 _) -> c0 == c)

--Rij
rulesAV :: [Rule] -> (Rule -> [Int]) -> Int -> [Rule]
rulesAV r a v = filter (elem v . a) r

--V
values :: [Rule] -> ([Rule] -> [Int]) -> Class -> [Int]
values r a c = (a . rules c) r

--DC
dontCare :: [Rule] -> ([Rule] -> [Int]) -> Int
dontCare r a = (maximum . a) r

--C
hasDontCare :: [Int] -> Int -> Int
hasDontCare v dc
    | dc `elem` v = 1
    | otherwise   = 0

--AE
attributeEffectiveness :: [Rule] -> ([Rule] -> Int) -> ([Rule] -> [Int]) -> Double
attributeEffectiveness r m a = fromIntegral (m r - sum ((map (\c -> hasDontCare (values r a c) (dontCare r a)) . classes) r)) / fromIntegral (m r)

--ADS
attributeDisjointnessScore :: [Rule] -> ([Rule] -> [Int]) -> Class -> Class -> Int
attributeDisjointnessScore r a c0 c1
    | intersect (values r a c0) (values r a c1) == values r a c0 = 0
    | intersect (values r a c0) (values r a c1) == values r a c1 = 1
    | (not . null) (values r a c0 `intersect` values r a c1) = 2
    | otherwise = 3

attributeDisjointness :: [Rule] -> ([Rule] -> [Int]) -> Int
attributeDisjointness r a = foldr (\x y -> y + foldr (\c d -> if x == c then d else d + attributeDisjointnessScore r a x c) 0 (classes r)) 0 (classes r)

maxAttributeDisjointness :: [Rule] -> [[Rule] -> [Int]] -> Int
maxAttributeDisjointness r = maximum . attributeDisjointnessList r

attributeDisjointnessList :: [Rule] -> [[Rule] -> [Int]] -> [Int]
attributeDisjointnessList r = map (attributeDisjointness r)

--AA
attributeAutonomy :: [Rule] -> [[Rule] -> [Int]] -> ([Rule] -> [Int]) -> Double
attributeAutonomy r as a = 1 / fromIntegral ((sum . map (attributeAutonomyValue r a as)) (a r))

attributeAutonomyValue :: [Rule] -> ([Rule] -> [Int]) -> [[Rule] -> [Int]] -> Int -> Int
attributeAutonomyValue r a as v
    | maxAttributeDisjointness (rulesAV r (\x -> a [x]) v) as == 0 = 0
    | (length as == 2) || (maxAttributeDisjointness (rulesAV r (\x -> a [x]) v) as `elem` attributeDisjointnessList (rulesAV r (\x -> a [x]) v) as) = 1
    | otherwise = succ ((pred . length) as * maxAttributeDisjointness (rulesAV r (\x -> a [x]) v) as - sum (attributeDisjointnessList (rulesAV r (\x -> a [x]) v) (deleteFirst (\x -> a r == x r) as)))

--MVD
minimumValueDistribution :: [Rule] -> ([Rule] -> [Int]) -> Double
minimumValueDistribution r a = fromIntegral $ (length . foldr (\x y -> values r a x `union` y) []) (classes r)

--utility
deleteFirst :: (a -> Bool) -> [a] -> [a]
deleteFirst f xs =
  foldr (\x r ~(_:tl) -> if f x then tl else x : r tl)
        (const []) xs xs

nElemIndex :: Eq a => a -> N.NonEmpty a -> Maybe Int
nElemIndex x (y N.:| ys) = elemIndex x (y:ys)

nConcat :: N.NonEmpty [a] -> [a]
nConcat = foldr1 (++)

nConcatMap :: (a -> [b]) -> N.NonEmpty a -> [b]
nConcatMap f (x N.:| xs) = foldr1 (++) (f x N.:| map f xs)

nConcatMap1 :: (a -> N.NonEmpty b) -> N.NonEmpty a -> N.NonEmpty b
nConcatMap1 f (x N.:| xs) = foldr1 (<>) (f x N.:| map f xs)

nDelete :: Eq a => a -> N.NonEmpty a -> [a]
nDelete x (y0 N.:| yn) = delete x (y0:yn)

toNonEmpty :: N.NonEmpty a -> [a]
toNonEmpty (x0 N.:| xn) = x0:xn

mustBeNonEmpty :: [a] -> N.NonEmpty a
mustBeNonEmpty (x0:xn) = x0 N.:| xn
mustBeNonEmpty [] = error "is empty"

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x