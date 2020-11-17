{-# LANGUAGE TupleSections #-}

module Rosby.Store.BTree where

import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

newtype Order = Order Int
  deriving (Eq, Show)

order :: Int -> Order
order x
  | x < 2     = Order 2
  | otherwise = Order x

data Node k v
  = Node
  { nodeKeys     :: Vector k
  , nodeChildren :: Vector (BTree k v)
  }
  deriving (Eq, Show)

node :: Order -> Vector k -> Vector (BTree k v) -> BTree k v
node o ks childs = BNode o $ Node ks childs

newtype Leaf k v = Leaf { leafValues :: Map k v }
  deriving (Eq, Show)

leaf :: Order -> Map k v -> BTree k v
leaf o vs = BLeaf o $ Leaf vs

data BTree k v
  = BNode Order (Node k v)
  | BLeaf Order (Leaf k v)
  deriving (Eq, Show)

insert :: Ord k => k -> v -> BTree k v -> BTree k v
insert k v = unzipper . moveToTop . insertWith k v . zipper

insertWith :: Ord k => k -> v -> Zipper k v -> Zipper k v
insertWith k v z@(BLeaf o@(Order o') (Leaf vs), cs)
  | M.size vs < o' = (BLeaf o $ Leaf (M.insert k v vs), cs)
  | otherwise =
    let (left, right) = M.spanAntitone (< k) vs
    in mergeUp (node o (V.singleton k) (V.fromList [leaf o left, leaf o right])) z
insertWith _ _ _ = undefined

zipper :: BTree k v -> Zipper k v
zipper = (, [])

mergeUp :: Ord k => BTree k v -> Zipper k v -> Zipper k v
mergeUp t z = maybe (merge t z) (merge t) . moveUp $ z

-- | Merge a @BNode@ with the currently focused node of the zipper,
-- ignore @BLeaf@ and return the input zipper
merge :: Ord k => BTree k v -> Zipper k v -> Zipper k v
merge (BLeaf _ _) z = z
merge _ z@(BLeaf _ _, _) = z
merge (BNode (Order o) (Node ks childs)) (BNode o' (Node ks' childs'), cs)
  | V.length childs' <= o - 2 =
    let (keyIndex, ks'') = insertKey (V.head ks) ks'
        childs'' = insertChilds keyIndex childs childs'
    in (BNode o' $ Node ks'' childs'', cs)
  | otherwise = undefined

insertKey :: Ord k => k -> Vector k -> (Int, Vector k)
insertKey key keys =
  let (left, right) = V.partition (< key) keys
  in (V.length left, left <> V.singleton key <> right)

insertChilds
  :: Int -- | Insert the "left" childen and "children" _around_ this index
  -> Vector (BTree k v) -- | The children to insert
  -> Vector (BTree k v) -- | The children to insert into
  -> Vector (BTree k v)
insertChilds idx cs csInto
  = V.slice 0 (idx - 1) csInto
  <> cs
  <> V.slice (idx + 1) (V.length csInto - 1) csInto

unzipper :: Zipper k v -> BTree k v
unzipper (t, _) = t

data Crumb k v = Down Order (Vector k) (Vector (BTree k v))
  deriving (Eq, Show)

type Zipper k v = (BTree k v, [Crumb k v])

moveDown :: Int -> Zipper k v -> Maybe (Zipper k v)
moveDown _ (BLeaf _ _, _) = Nothing
moveDown idx (BNode o (Node ks childs), cs) =
  (, Down o ks childs:cs) <$> childs !? idx

moveUp :: Zipper k v -> Maybe (Zipper k v)
moveUp (_, []) = Nothing
moveUp (_, Down o ks childs:cs) = pure (BNode o $ Node ks childs, cs)

moveToTop :: Zipper k v -> Zipper k v
moveToTop z = case moveUp z of
  Nothing -> z
  Just z' -> moveToTop z'

-- examples

example1 :: BTree Int Char
example1 = BNode (order 4)
  $ Node (V.fromList [3, 4])
  (V.fromList
   [ BLeaf (order 4) $ Leaf (M.fromList [(1, 'a'), (2, 'b')])
   , BLeaf (order 4) $ Leaf (M.fromList [(4, 'c'), (5, 'd')])
   ])
