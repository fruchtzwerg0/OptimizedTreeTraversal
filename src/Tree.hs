module Tree where

import Data.List
import qualified Data.List.NonEmpty as N

data Size = Size Int Int Int deriving Show
data Material = Wood | Metal | Glass | NoMaterial deriving (Eq, Show)

allMaterials :: [Material]
allMaterials = [Wood, Metal, Glass]

--data Constraint = SizeConstraint Int Int Int Int (Int -> Int -> Int -> Bool) | CapacityConstraint Int Int Int (Int -> Bool) | MaterialConstraint Int [Material] (Material -> Bool)

data Constraint = SizeConstraint Int Int Int Int (Int -> Int -> Int -> Bool) |
                  CapacityConstraint Int Int Int (Int -> Bool) |
                  MaterialConstraint Int [Material] (Material -> Bool) |
                  NoConstraint

instance Show Constraint where
  show (SizeConstraint c x y z _) =
    "SizeConstraint: " ++ show c ++ " " ++ show x ++ " " ++ show y ++ " " ++ show z
  show (CapacityConstraint c xc x _) =
    "CapacityConstraint: " ++ show c ++ " " ++ show xc ++ " " ++ show x
  show (MaterialConstraint c x _) =
    "MaterialConstraint: " ++ show c ++ " " ++ show x

instance Monoid Constraint where
    mempty = NoConstraint
instance Semigroup Constraint where
    (CapacityConstraint _ s c _) <> (CapacityConstraint _ s' c' _) =
        CapacityConstraint 0 (min s s') (min c c') (<= min c c')
    (SizeConstraint _ x y z _) <> (SizeConstraint _ x' y' z' _) =
        SizeConstraint 0 (min x x') (min y y') (min z z') (\x''' y''' z''' -> x''' <= min x x' && y''' <= min y y' && z''' <= min z z')
    (MaterialConstraint _ ms _) <> (MaterialConstraint _ ms' _) =
        MaterialConstraint 0 (ms `intersect` ms') (`elem` intersect ms ms')
    NoConstraint <> x = x
    x <> NoConstraint = x
    _ <> _ = error "Not possible"

(><) :: Constraint -> Constraint -> Constraint
(CapacityConstraint _ s c _) >< (CapacityConstraint _ s' c' _) =
    CapacityConstraint 0 (s + s') (c + c') (<= (c + c'))
(SizeConstraint _ x y z _) >< (SizeConstraint _ x' y' z' _) =
    SizeConstraint 0 (max x x') (max y y') (max z z') (\x''' y''' z''' -> x''' <= max x x' && y''' <= max y y' && z''' <= max z z')
(MaterialConstraint _ ms _) >< (MaterialConstraint _ ms' _) =
    MaterialConstraint 0 (ms `union` ms') (`elem` union ms ms')
NoConstraint >< x = x
x >< NoConstraint = x
_ >< _ = error "Not possible"

data Item = Item Material Size deriving Show
data InventoryTree = Leaf String Order [Constraint] Int Int [Item] | Node String Order [Constraint] Int Int [InventoryTree] deriving Show
data InventoryTreeSecond = LeafS String Order [Constraint] Int Int [Item] | NodeS String Order [Constraint] Int Int (N.NonEmpty InventoryTreeSecond) deriving Show
type Order = Int

testTree1 :: InventoryTree
testTree1 = Node "Root" 0 [] 1 1
                [
                    Node "RowA" 0 [SizeConstraint 0 10 9 2 (\x y z -> x <= 10 && y <= 9 && z <= 2),
                                   MaterialConstraint 0 [Glass] (`elem` [Glass])] 1 1
                        [
                            Node "ColA" 0 [] 1 1
                                [
                                    Leaf "BinA" 0 [SizeConstraint 0 8 7 2 (\x y z -> x <= 8 && y <= 7 && z <= 2),
                                                   CapacityConstraint 0 4 5 (<= 5)] 1 1
                                                   [Item Glass (Size 4 4 2),Item Glass (Size 7 3 1),
                                                    Item Glass (Size 6 5 1),Item Glass (Size 8 7 1)]
                                ],
                            Node "ColB" 1 [SizeConstraint 0 5 4 1 (\x y z -> x <= 5 && y <= 4 && z <= 1)] 1 1
                                [
                                    Leaf "BinB" 0 [CapacityConstraint 0 1 6 (<= 6)] 1 1 [Item Glass (Size 5 3 1)],
                                    Leaf "BinC" 1 [CapacityConstraint 0 1 4 (<= 4)] 1 1 [Item Glass (Size 3 2 1)]
                                ],
                            Node "ColC" 2 [] 1 1
                                [
                                    Leaf "BinD" 0 [CapacityConstraint 0 8 10 (<= 10)] 1 1 [Item Glass (Size 10 3 2),Item Glass (Size 4 3 1),
                                                                                           Item Glass (Size 9 7 2),Item Glass (Size 6 3 2),
                                                                                           Item Glass (Size 9 7 2),Item Glass (Size 9 8 2),
                                                                                           Item Glass (Size 8 5 2),Item Glass (Size 6 4 1)]
                                ]
                        ],
                    Node "RowB" 1 [SizeConstraint 0 10 8 2 (\x y z -> x <= 10 && y <= 8 && z <= 2),
                                   MaterialConstraint 0 [Metal] (`elem` [Metal])] 1 1
                        [
                            Node "ColD" 0 [] 1 1
                                [
                                    Leaf "BinE" 0 [CapacityConstraint 0 6 8 (<= 8)] 1 1 [Item Metal (Size 7 5 1),Item Metal (Size 6 1 1),
                                                                                         Item Metal (Size 8 8 2),Item Metal (Size 8 5 1),
                                                                                         Item Metal (Size 9 8 1),Item Metal (Size 7 3 1)],
                                    Leaf "BinF" 1 [CapacityConstraint 0 6 9 (<= 9)] 1 1 [Item Metal (Size 10 2 1),Item Metal (Size 8 2 1),
                                                                                         Item Metal (Size 10 7 2),Item Metal (Size 8 6 1),
                                                                                         Item Metal (Size 7 5 1),Item Metal (Size 10 6 1)],
                                    Leaf "BinG" 2 [SizeConstraint 0 5 4 1 (\x y z -> x <= 5 && y <= 4 && z <= 1),
                                                   CapacityConstraint 0 1 11 (<= 11)] 1 1 [Item Metal (Size 3 2 1)]
                                ]
                        ],
                    Node "RowC" 2 [SizeConstraint 0 5 4 1 (\x y z -> x <= 5 && y <= 4 && z <= 1),
                                   CapacityConstraint 0 0 50 (<= 50)] 1 1
                        [
                            Node "ColE" 0 [MaterialConstraint 0 [Wood] (`elem` [Wood])] 1 1
                                [
                                    Leaf "BinH" 0 [CapacityConstraint 0 0 12 (<= 12)] 1 1 []
                                ],
                            Node "ColF" 1 [SizeConstraint 0 5 4 1 (\x y z -> x <= 5 && y <= 4 && z <= 1),
                                           MaterialConstraint 0 [Metal] (`elem` [Metal])] 1 1
                                [
                                    Leaf "BinI" 0 [CapacityConstraint 0 1 15 (<= 15)] 1 1 [Item Metal (Size 5 4 1)]
                                ],
                            Node "ColG" 2 [SizeConstraint 0 4 4 1 (\x y z -> x <= 4 && y <= 4 && z <= 1),
                                           MaterialConstraint 0 [Glass, Wood] (`elem` [Glass, Wood])] 1 1
                                [
                                    Leaf "BinJ" 0 [CapacityConstraint 0 0 14 (<= 14)] 1 1 []
                                ]
                        ],
                    Node "RowD" 3 [SizeConstraint 0 10 8 2 (\x y z -> x <= 10 && y <= 8 && z <= 2),
                                   MaterialConstraint 0 [Wood, Glass] (`elem` [Wood, Glass])] 1 1
                        [
                            Node "ColH" 0 [SizeConstraint 0 6 5 1 (\x y z -> x <= 6 && y <= 5 && z <= 1),
                                           MaterialConstraint 0 [Wood] (`elem` [Wood])] 1 1
                                [
                                    Leaf "BinK" 0 [CapacityConstraint 0 4 20 (<= 20)] 1 1 [Item Wood (Size 4 3 1),Item Wood (Size 4 2 1),
                                                                                           Item Wood (Size 6 2 1),Item Wood (Size 4 1 1)]
                                ],
                            Node "ColI" 1 [SizeConstraint 0 9 8 2 (\x y z -> x <= 9 && y <= 8 && z <= 2),
                                           CapacityConstraint 0 15 20 (<= 20)] 1 1
                                [
                                    Leaf "BinL" 0 [CapacityConstraint 0 9 12 (<= 12)] 1 1 [Item Glass (Size 7 1 1),Item Glass (Size 7 6 2),
                                                                                           Item Wood (Size 7 7 2),Item Glass (Size 5 5 2),
                                                                                           Item Glass (Size 7 4 1),Item Glass (Size 7 3 1),
                                                                                           Item Glass (Size 9 8 1),Item Wood (Size 8 7 1),
                                                                                           Item Wood (Size 9 4 2)],
                                    Leaf "BinM" 1 [] 1 1 [Item Wood (Size 9 3 2),Item Wood (Size 9 2 2),
                                                          Item Glass (Size 8 5 1),Item Wood (Size 9 5 2),
                                                          Item Glass (Size 7 4 1),Item Glass (Size 6 3 2)]
                                ]
                        ],
                    Node "RowE" 4 [SizeConstraint 0 10 9 2 (\x y z -> x <= 10 && y <= 9 && z <= 2),
                                   MaterialConstraint 0 [Metal] (`elem` [Metal])] 1 1
                        [
                            Node "ColJ" 0 [SizeConstraint 0 9 9 1 (\x y z -> x <= 9 && y <= 9 && z <= 1),
                                           CapacityConstraint 0 8 30 (<= 30)] 1 1
                                [
                                    Leaf "BinN" 0 [SizeConstraint 0 5 4 1 (\x y z -> x <= 5 && y <= 4 && z <= 1)] 1 1 [Item Metal (Size 1 1 3)],
                                    Leaf "BinO" 1 [CapacityConstraint 0 7 10 (<= 10)] 1 1 [Item Metal (Size 9 9 1),Item Metal (Size 8 4 1),
                                                                                           Item Metal (Size 9 4 1),Item Metal (Size 9 6 1),
                                                                                           Item Metal (Size 7 1 1),Item Metal (Size 9 6 1),
                                                                                           Item Metal (Size 5 4 1)]
                                ]
                        ]
                ]
