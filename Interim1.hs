module Radix where
  
data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
            deriving (Eq, Show)

type IntTree = Tree Int

type RadixTree = Tree Bool

type BitString = [Int]

--------------------------------------------------------------------------

buildIntTree :: [Int] -> IntTree
buildIntTree
  = foldr add Empty
  where
    add x Empty
      = Leaf x
    add x (Leaf y)
      = add x (Node y Empty Empty)
    add x t@(Node y l r)
      | x == y    = t
      | x < y     = Node y (add x l) r
      | otherwise = Node y l (add x r)

--------------------------------------------------------------------------

a, m :: Integer
m = 1073741824
a = 16387

rand :: Integer -> [Double]
rand s
  = fromInteger s / fromInteger m : rand s' where s' = (s * a) `mod` m

randomInts :: Int -> Int -> Integer -> [Int]
randomInts m n s
  = take m (map (round . (+1) . (* fromIntegral n)) (rand s))

rs :: [Int]
rs = randomInts 1000 500 765539

--------------------------------------------------------------------------
-- Pre (universal): all integers are non-negative

size :: IntTree -> Int
size (Node _ left right) = 13 + size left + size right
size (Leaf _) = 5
size Empty = 1


size' :: RadixTree -> Int
size' (Node _ left right) = 9 + size' left + size' right
size' (Leaf _) = 1


binary :: Int -> BitString
binary x
  = bin x []
  where 
    bin :: Int -> [Int] -> [Int]
    bin 0 curr = 0 : curr
    bin 1 curr = 1 : curr
    bin x curr 
      = bin a (b : curr)
      where
        (a, b) = quotRem x 2


insert :: BitString -> RadixTree -> RadixTree
insert [] (Node _ left right) = Node True left right
insert [] (Leaf _) = Leaf True
insert (a : as) (Node bool left right)
  | a == 0    = Node bool (insert as left) right
  | otherwise = Node bool left (insert as right)
insert a (Leaf bool) = insert a (Node bool (Leaf False) (Leaf False))


buildRadixTree :: [Int] -> RadixTree
buildRadixTree = foldr (insert . binary) (Leaf False)


member :: Int -> RadixTree -> Bool
member x
  = help (binary x)
  where 
    help :: [Int] -> RadixTree -> Bool
    help [_] (Node a _ _)
      | not a     = False
      | otherwise = True
    help [_] (Leaf a)
      | not a     = False
      | otherwise = True
    help (b : bs) (Node _ left right)
      | b == 1 = help bs right
      | b == 0 = help bs left


union :: RadixTree -> RadixTree -> RadixTree
union (Leaf a) (Leaf b)
  | a || b    = Leaf True
  | otherwise = Leaf False
union (Node a left right) (Leaf b)
  | a || b    = Node True left right
  | otherwise = Node False left right
union (Leaf a) (Node b left right)
  | a || b    = Node True left right
  | otherwise = Node False left right
union (Node a left1 right1) (Node b left2 right2)
  | a || b    = Node True (left1 `union` left2) (right1 `union` right2)
  | otherwise = Node False (left1 `union` left2) (right1 `union` right2)

intersection :: RadixTree -> RadixTree -> RadixTree
intersection (Leaf a) (Leaf b)
  | a && b    = Leaf True
  | otherwise = Leaf False
intersection (Node a left right) (Leaf b)
  | a && b    = Node True left right
  | otherwise = Node False left right
intersection (Leaf a) (Node b left right)
  | a && b    = Node True left right
  | otherwise = Node False left right
intersection (Node a left1 right1) (Node b left2 right2)
  | a && b    = Node True (left1 `intersection` left2) (right1 `intersection` right2)
  | otherwise = Node False (left1 `intersection` left2) (right1 `intersection` right2)


-- CONCLUSION: The break-even point is xxx.

-----------------------------------------------------------------------------
-- Some test trees...

figure :: RadixTree
figure
  = Node False (Leaf True)
               (Node True (Leaf False)
                          (Node True (Node False (Leaf True)
                                                 (Leaf False))
                                     (Leaf True)))

t1 :: IntTree
t1 = Node 20 (Node 8 Empty
                     (Node 12 Empty
                              Empty))
             Empty

t2 :: RadixTree
t2 = Node False (Node False (Leaf True)
                            (Node True (Leaf False) (Leaf True)))
                (Leaf True)

