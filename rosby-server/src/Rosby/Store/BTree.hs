{-# LANGUAGE TupleSections #-}

module Rosby.Store.BTree where

import Data.List
import Data.Vector (Vector, (!?), (//))
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

insertLeaf :: (Ord k, Ord v) => k -> v -> BTree k v -> Maybe (BTree k v)
insertLeaf k v (BLeaf (Order o) (Leaf ks vs))
  | V.length ks == V.length vs && V.length ks < o =
    Just $ leaf (order o) $ V.toList $ insertInto k v (V.zip ks vs)
  | otherwise = Nothing
  where
    insertInto :: (Ord k, Ord v) => k -> v -> Vector (k, v) -> Vector (k, v)
    insertInto k' v' = V.fromList . sort . (:) (k', v') . V.toList
insertLeaf _ _ _ = Nothing

-- insert :: (Eq k, Ord k, Show k, Show v) => k -> v -> BTree k v -> BTree k v
-- insert k v = unzipper . insertWith k v . zipper

-- insertWith :: (Ord k, Show k, Show v) => k -> v -> Zipper k v -> Zipper k v
--   insertWith k v z@(BLeaf o@(Order o') (Leaf ks vs)), cs)
--   | M.size vs < o' = (BLeaf o $ insertLeaf , cs)
--   | otherwise =
--     let allKeys = V.fromList . sort $ (k : M.keys vs)
--         mid = V.length allKeys `div` 2
--     in case allKeys !? mid of
--       Nothing -> undefined
--       Just nk ->
--         let (left, right) = M.partitionWithKey (\x _ -> x < nk) $ M.insert k v vs
--         in mergeUp (node o (V.singleton nk) (V.fromList [leaf o left, leaf o right])) z
-- insertWith _ _ _ = undefined

zipper :: (Eq k, Ord k, Show k) => BTree k v -> Zipper k v
zipper = (, [])

mergeUp :: Ord k => BTree k v -> Zipper k v -> Zipper k v
mergeUp t z = maybe (merge t z) (merge t) . moveUp $ z

mergeNode
  :: (Ord k, Ord v)
  => BTree k v         -- ^ The node to merge with
  -> BTree k v         -- ^ The node to merge
  -> Maybe (BTree k v) -- ^ We can only merge @BNode@ and return
                       -- @Nothing@ in all other cases
mergeNode (BNode o (Node keys childs)) (BNode o' (Node keys' childs'))
  | o /= o'   = Nothing
  | otherwise =
      Just $ node o mergedKeys mergedChilds
  where
    mergedKeys = V.fromList . sort . V.toList $ V.concat [keys, keys']
    mergedChilds = V.fromList . nub . sort . V.toList $ V.concat [childs, childs']
mergeNode _ _ = Nothing

-- | Merge a @BNode@ with the currently focused node of the zipper,
-- ignore @BLeaf@ and return the input zipper
merge :: Ord k => BTree k v -> Zipper k v -> Zipper k v
merge (BLeaf _ _) z = z
merge _ z@(BLeaf _ _, _) = z
merge (BNode (Order o) (Node ks childs)) z@(BNode o' (Node ks' childs'), cs)
  | V.length childs' <= o - 2 =
    let (keyIndex, ks'') = insertKey (V.head ks) ks'
        childs'' = insertChilds keyIndex childs childs'
    in (BNode o' $ Node ks'' childs'', cs)
  | otherwise =
    mergeUp (BNode o'
             (Node
              (V.singleton middleKey)
              (V.fromList [ node o' (V.takeWhile (< middleKey) ks') (V.take (middleKeyIndex childs' + 1) childs')
                          , node o' (V.dropWhile (<= middleKey) ks') (V.drop (middleKeyIndex childs' + 1) childs')
                          ]) )) z
  where
    middleKeyIndex :: Vector a -> Int
    middleKeyIndex v = V.length v `div` 2

    middleKey :: k
    middleKey = undefined

    -- middleKey :: Int -> Vector a -> Maybe k
    -- middleKey x v = x !? v

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


-- mergeTree :: Ord k => BTree k v -> BTree k v -> BTree k v
-- mergeTree _ t = t
-- mergeTree
--   (BNode _ (Node mergeKeys mergeChilds))
--   (BNode _ (Node parentKeys parentChilds))
--   = let totesKeys = V.fromList . sort . V.toList $ ((V.++) mergeKeys parentKeys)
--         totesChilds = mergeChilds V.++ parentChilds
--     in case V.head totesChilds of
--          -- Assume that the vectors are homogenous...
--          BNode _ _ -> _
--          BLeaf _ _  -> _
