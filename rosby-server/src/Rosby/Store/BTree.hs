{-# LANGUAGE TupleSections #-}

module Rosby.Store.BTree where

import Data.List hiding (find)
import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as V

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

instance (Ord k, Eq v) => Ord (Node k v) where
  Node k _ <= Node k' _ = k <= k'

node :: Order -> Vector k -> Vector (BTree k v) -> BTree k v
node o ks childs = BNode o $ Node ks childs

data Leaf k v
  = Leaf (Vector k) (Vector v)
  deriving (Eq, Show)

instance (Ord k, Eq v) => Ord (Leaf k v) where
  Leaf k _ <= Leaf k' _ = k <= k'

leaf :: Order -> [(k, v)] -> BTree k v
leaf o kvs =
  let ks = V.fromList . map fst $ kvs
      vs = V.fromList . map snd $ kvs
  in BLeaf o $ Leaf ks vs

data BTree k v
  = BNode Order (Node k v)
  | BLeaf Order (Leaf k v)
  deriving (Eq, Show)

instance (Ord k, Ord v) => Ord (BTree k v) where
  BNode _ n <= BNode _ n' = n <= n'
  BLeaf _ l <= BLeaf _ l' = l <= l'
  BNode _ (Node n _) <= BLeaf _ (Leaf l _) = n <= l
  BLeaf _ (Leaf l _) <= BNode _ (Node n _) = l <= n

data InsertResult a
  = Overflow a
  -- ^ Case where we inserted and the order property is violated
  | Underflow a
  -- ^ Case where we have inserted into a Leaf and order property is
  -- preserved
  | NoInsert
  -- ^ Cannot insert into a BNode
  deriving (Eq, Show)

insertLeaf :: (Ord k, Ord v) => k -> v -> Zipper k v -> InsertResult (BTree k v)
insertLeaf k v (BLeaf (Order o) (Leaf ks vs), _)
  | V.length ks == V.length vs && V.length ks < o =
    Underflow $ leaf (order o) $ V.toList $ insertInto k v (V.zip ks vs)
  | otherwise = Overflow $ splitNode k v ks vs
  where
    insertInto :: (Ord k, Ord v) => k -> v -> Vector (k, v) -> Vector (k, v)
    insertInto k' v' = V.fromList . sort . (:) (k', v') . V.toList
    splitNode :: (Ord k, Ord v) => k -> v -> Vector k -> Vector v -> BTree k v
    splitNode key value keys values =
      -- TODO: insert key and value into keys and values before splitting them
      let (leftKeys, rightKeys) = V.partition (< key) keys
          (leftValues, rightValues) = V.splitAt (V.length leftKeys) values
          k' = V.head rightKeys
      in node (order o) (V.singleton k')
         $ V.fromList [ BLeaf (order o) (Leaf leftKeys leftValues)
                      , BLeaf (order o) (Leaf rightKeys rightValues)
                      ]
insertLeaf _ _ _ = NoInsert

zipper :: (Eq k, Ord k, Show k) => BTree k v -> Zipper k v
zipper = (, [])

mergeUp :: (Ord k, Ord v) => BTree k v -> Zipper k v -> Maybe (Zipper k v)
mergeUp (BNode _ (Node childKeys grandchildren)) (_, DownTo idx o keys childs:cs) = do
  let (left, right) = V.splitAt idx childs
      right' = if V.length right > 0 then V.tail right else right
  pure (node o (mergeKeys keys childKeys) (V.concat [left, grandchildren, right']), cs)
mergeUp _ (_, _) = Nothing

mergeKeys :: Ord a => Vector a -> Vector a -> Vector a
mergeKeys xs ys = V.fromList . sort . V.toList $ V.concat [xs, ys]

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
unzipper z = case moveUp z of
  Nothing -> fst z
  Just z' -> unzipper z'

data Crumb k v = DownTo Int Order (Vector k) (Vector (BTree k v))
  deriving (Eq, Show)

type Zipper k v = (BTree k v, [Crumb k v])

moveDown :: Int -> Zipper k v -> Maybe (Zipper k v)
moveDown _ (BLeaf _ _, _) = Nothing
moveDown idx (BNode o (Node ks childs), cs) =
  (, DownTo idx o ks childs:cs) <$> childs !? idx

moveUp :: Zipper k v -> Maybe (Zipper k v)
moveUp (_, []) = Nothing
moveUp (t, DownTo idx o keys childs:cs) = Just (node o keys childs', cs)
  where
    childs' = childs // [(idx, t)]

find :: Ord k => k -> Zipper k v -> Maybe (Zipper k v)
find _ z@(BLeaf _ _, _) = Just z
find key z@(BNode _ (Node keys _), _) = moveDown (search key keys 0 (V.length keys - 1)) z
  where
    search :: Ord k => k -> Vector k -> Int -> Int -> Int
    search k ks lo hi =
      if lo == hi
      then
        case compare k (ks ! lo) of
          LT -> lo
          GT -> lo + 1
          EQ -> undefined
      else
        let mid = lo + ((hi - lo) `div` 2)
        in case compare k (ks ! mid) of
          LT -> search k ks lo mid
          GT -> search k ks (mid + 1) hi
          EQ -> mid + 1

moveToTop :: Zipper k v -> Maybe (Zipper k v)
moveToTop z = case moveUp z of
  Nothing -> Just z
  Just z' -> moveToTop z'

insertWithMerge :: (Ord k, Ord v, Show k) => k -> v -> Zipper k v -> Maybe (Zipper k v)
insertWithMerge key value z@(_, cs) = do
  lf <- find key z
  case insertLeaf key value lf of
    Overflow bnode -> mergeUp bnode lf
    Underflow bnode -> moveToTop (bnode, cs)
    NoInsert -> Nothing -- TODO: maybe error? this would be a malformed tree.

-- -- TODO: We should remove the Maybe
-- insert :: (Ord k, Show k) => k -> v -> BTree k v -> BTree k v
-- insert key value root = fmap fst <$> insertWithMerge key value <=< find key $ zipper root
