{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}

module Rosby.Store.BTree where

import Data.List
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

-- The degree of of B+ Tree is `t >= 2`

newtype Order = Order Int
  deriving (Eq, Show)
  deriving Num via Int

mkOrder :: Int -> Order
mkOrder x
  | x < 2     = Order 2
  | otherwise = Order x

-- Invariant:
--   Can't have less than `order/2` children
--     Exception: when it's the root, at least 2 children
data Node k v
  = Node
  { nodeOrder :: Order
  , nodeKeys  :: Vector k
  , nodeChildren :: Vector (BTreeNode k v)
  }
  deriving (Eq, Show)

data Leaf k v
  = Leaf
  { leafOrder   :: Order
  , leafKeys    :: Vector k
  -- ^ The indexes in leafKeys relate to the indexes in leafValues
  , leafValues  :: Vector v
  , leafSibling :: Maybe (Leaf k v)
  }
  deriving (Eq, Show)

data BTreeNode k v
  = BNode (Node k v)
  | BLeaf (Leaf k v)
  deriving (Eq, Show)

newtype BTree k v = BTree { getRoot :: BTreeNode k v }
  deriving (Eq, Show)

empty :: Int -> BTree k v
empty order = BTree $ BLeaf $ Leaf (mkOrder order) V.empty V.empty Nothing

insert :: BTree k v -> k -> v -> BTree k v
insert (BTree (BLeaf leaf@(Leaf (Order order) keys values _))) key value
  | (V.length keys) < order =
    BTree $ BLeaf $ leaf
    { leafKeys = V.cons key keys
    , leafValues = V.cons value values
    }
  | otherwise = _

splitLeaf :: (Ord k, Ord v) => Leaf k v -> k -> v -> Node k v
splitLeaf (Leaf (Order order) ks vs _) k v =
  if k < ks ! (midPoint order ks)
  then
    let (lowerKeys, upperKeys) = V.splitAt (midPoint order ks) ks
        (lowerValues, upperValues) = V.splitAt (midPoint order vs) vs
        lowerKeys' = insertInto k lowerKeys
        lowerValues' = insertInto v lowerValues
        lower  = Leaf (mkOrder order) lowerKeys' lowerValues' (Just upper)
        upper = Leaf (mkOrder order) upperKeys upperValues Nothing
    in Node (mkOrder order) (V.singleton $ V.head upperKeys) $ V.fromList [BLeaf lower, BLeaf upper]
  else
    let (lowerKeys, upperKeys) = V.splitAt (midPoint order ks) ks
        (lowerValues, upperValues) = V.splitAt (midPoint order vs) vs
        upperKeys' = insertInto k upperKeys
        upperValues' = insertInto v upperValues
        lower  = Leaf (mkOrder order) lowerKeys lowerValues (Just upper)
        upper = Leaf (mkOrder order) upperKeys' upperValues' Nothing
    in Node (mkOrder order) (V.singleton $ V.head upperKeys) $ V.fromList [BLeaf lower, BLeaf upper]
  where
    midPoint order xs = fromIntegral . floor $ (realToFrac order) / 2
    insertInto :: Ord a => a -> Vector a -> Vector a
    insertInto x xs = V.fromList . sort . ((:) x) . V.toList $ xs
