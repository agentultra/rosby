module Rosby.Store.BTree where

import Data.Vector

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
  , leafValues  :: Vector v
  , leafSibling :: Maybe (Leaf k v)
  }
  deriving (Eq, Show)

data BTreeNode k v
  = BNode (Node k v)
  | BLeaf (Leaf k v)
  deriving (Eq, Show)

newtype BTree k v = BTree { getRoot :: Node k v }
