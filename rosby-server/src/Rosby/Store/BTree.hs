module Rosby.Store.BTree where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

data Node k v
  = Node
  { nodeOrder :: Int
  , nodeKeys :: Vector k
  , nodeChildren :: Vector (BTreeNode k v)
  }
  deriving (Eq, Show)

data Leaf k v
  = Leaf
  { leafOrder   :: Int
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
empty order = BTree $ BLeaf $ Leaf order V.empty V.empty Nothing

insert :: BTree k v -> k -> v -> BTree k v
insert (BTree (BLeaf leaf@(Leaf order keys values _))) key value
  | (V.length keys) < order =
    BTree $ BLeaf $ leaf
    { leafKeys = V.cons key keys
    , leafValues = V.cons value values
    }
  | otherwise = _

splitLeaf :: (Ord k, Ord v) => Leaf k v -> k -> v -> Node k v
splitLeaf (Leaf order ks vs _) k v =
  if k < ks ! (midPoint order ks)
  then
    let (leftKeys, rightKeys) = V.splitAt (midPoint order ks) ks
        (leftValues, rightValues) = V.splitAt (midPoint order vs) vs
        (leftKeys', leftValues') = insertInto k v leftKeys leftValues
        -- TODO (james): remember the x node key
        left  = Leaf order leftKeys' leftValues' (Just right)
        right = Leaf order rightKeys rightValues Nothing
    in Node order _ $ V.fromList [BLeaf left, BLeaf right]
  else _
  where
    midPoint order xs = fromIntegral . ceiling $ (realToFrac order) / 2
    insertInto = _
