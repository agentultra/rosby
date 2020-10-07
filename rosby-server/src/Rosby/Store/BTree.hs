{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data Nat = Z | S Nat

-- Invariant:
--   Can't have less than `order/2` children
--     Exception: when it's the root, at least 2 children
--   Keys must maintain Ord order
data Node h k v
  = Node
  { nodeKeys     :: Vector k
  , nodeChildren :: Vector (BTree h k v)
  }

data Leaf k v
  = Leaf
  { leafKeys    :: Vector k
  -- ^ The indexes in leafKeys relate to the indexes in leafValues
  , leafValues  :: Vector v
  }
  deriving (Eq, Show)

data BTree h k v where
  BNode :: Node h k v -> BTree ('S h) k v
  BLeaf :: BTree 'Z k v

data Tree k v where
  Tree :: BTree h k v -> Tree k v

type Keep t h k v = BTree h k v -> t
type Split t h k v = k -> Vector (BTree h k v) -> Vector (BTree h k v) -> t

node :: Ord k => k -> Vector (BTree h k v) -> Vector (BTree h k v) -> Node h k v
node key left right = Node (V.singleton key) $ (V.++) left right

insert :: Ord k => k -> v -> Tree k v -> Tree k v
insert key value (Tree tree) =
  insert' key value tree Tree (\k left right -> Tree (BNode $ node k left right))
  where
    insert' :: forall h k t p v. Ord k
            => k
            -> v
            -> BTree h k v
            -> Keep t h k v
            -> Split t p k v
            -> Tree k v
    insert' k v BLeaf keep split = undefined
    insert' k v (BNode n) keep split = ins k v n
      where
        ins :: k -> v -> Node m k v -> Node p k v
        ins = undefined























-- empty :: Int -> BTree k v
-- empty order = BTree $ BLeaf $ Leaf (mkOrder order) V.empty V.empty Nothing

-- insert :: (Ord k, Ord v) => BTree k v -> k -> v -> BTree k v
-- insert (BTree (BLeaf leaf@(Leaf (Order order) keys values _))) key value
--   | (V.length keys) < (order - 1) =
--     BTree $ BLeaf $ leaf
--     { leafKeys = V.fromList . sort . V.toList . V.cons key $ keys
--     , leafValues = V.cons value values
--     }
--   | otherwise = BTree $ BNode $ splitLeaf leaf key value

-- splitLeaf :: (Ord k, Ord v) => Leaf k v -> k -> v -> Node k v
-- splitLeaf (Leaf (Order order) ks vs _) k v =
--   if k < ks ! (midPoint order ks)
--   then
--     let (lowerKeys, upperKeys) = V.splitAt (midPoint order ks) ks
--         (lowerValues, upperValues) = V.splitAt (midPoint order vs) vs
--         lowerKeys' = insertInto k lowerKeys
--         lowerValues' = insertInto v lowerValues
--         lower  = Leaf (mkOrder order) lowerKeys' lowerValues' (Just upper)
--         upper = Leaf (mkOrder order) upperKeys upperValues Nothing
--     in Node (mkOrder order) (V.singleton $ V.head upperKeys) $ V.fromList [BLeaf lower, BLeaf upper]
--   else
--     let (lowerKeys, upperKeys) = V.splitAt (midPoint order ks) ks
--         (lowerValues, upperValues) = V.splitAt (midPoint order vs) vs
--         upperKeys' = insertInto k upperKeys
--         upperValues' = insertInto v upperValues
--         lower  = Leaf (mkOrder order) lowerKeys lowerValues (Just upper)
--         upper = Leaf (mkOrder order) upperKeys' upperValues' Nothing
--     in Node (mkOrder order) (V.singleton $ V.head upperKeys) $ V.fromList [BLeaf lower, BLeaf upper]
--   where
--     midPoint order xs = fromIntegral . floor $ (realToFrac order) / 2
--     insertInto :: Ord a => a -> Vector a -> Vector a
--     insertInto x xs = V.fromList . sort . ((:) x) . V.toList $ xs
