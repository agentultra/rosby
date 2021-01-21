module Rosby.Store.BTreeSpec where

import Control.Monad
import Data.Maybe
import Test.Hspec
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Rosby.Store.BTree

spec :: Spec
spec = do
  context "Rosby.Store.BTree" $ do
    describe "zipper" $ do
      it "zipper identity" $ do
        (unzipper . zipper $ leaf (order 3) [('A', 0)])
          `shouldBe`
          (leaf (order 3) [('A', 0)])
      it "moving down then back to top returns the root" $ do
        let root = node (order 3) (V.fromList ['C'])
              $ V.fromList
              [ leaf (order 3) [('A', 0), ('B', 1)]
              , leaf (order 3) [('C', 2), ('D', 3)]
              ]
        unzipper (fromMaybe undefined (moveDown 0 $ zipper root))
          `shouldBe`
          root

      it "should move down, up, then unzip to the root" $ do
        let root = node (order 3) (V.fromList ['C'])
              $ V.fromList
              [ leaf (order 3) [('A', 0), ('B', 1)]
              , leaf (order 3) [('C', 2), ('D', 3)]
              ]
        unzipper (fromMaybe undefined (moveUp <=< moveDown 0 $ zipper root))
          `shouldBe`
          root

    describe "mergeNode" $ do
      it "should preserve the order property" $ do
        let root = node (order 3) (V.singleton 'D')
              $ V.fromList [ leaf (order 3) [('A', 0), ('B', 1), ('C', 2)]
                         , leaf (order 3) [('D', 3), ('E', 4), ('F', 5)]
                         ]
            node' = node (order 3) (V.singleton 'G')
              $ V.fromList [ leaf (order 3) [('D', 3), ('E', 4), ('F', 5)]
                           , leaf (order 3) [('G', 6)]
                           ]

        (mergeNode root node')
          `shouldBe`
          Just (node (order 3) (V.fromList ['D', 'G'])
                $ V.fromList [ leaf (order 3) [('A', 0), ('B', 1), ('C', 2)]
                             , leaf (order 3) [('D', 3), ('E', 4), ('F', 5)]
                             , leaf (order 3) [('G', 6)]
                             ])

      describe "insertLeaf" $ do
        it "should preserve the order property" $ do
          let tree = leaf (order 3) [('B', 1)]
          (insertLeaf 'A' 0 tree)
            `shouldBe`
            (Just $ leaf (order 3) [('A', 0), ('B', 1)])

        it "should order on the keys all else being equal" $ do
          let tree = leaf (order 3) [('B', 1)]
          (insertLeaf 'A' 1 tree)
            `shouldBe`
            (Just $ leaf (order 3) [('A', 1), ('B', 1)])

    -- describe "insert" $ do
    --   it "should keep a value inserted into an empty root" $ do
    --     let root = leaf (order 3) []
    --     insert 'A' 0 root
    --       `shouldBe`
    --       (leaf (order 3) [('A', 0)])

    --   it "should split a root leaf into a node" $ do
    --     let root = leaf (order 3) [('A', 0), ('B', 1), ('C', 2)]
    --     insert 'D' 3 root
    --     `shouldBe`
    --       (node (order 3) (V.fromList ['C'])
    --        $ V.fromList [ leaf (order 3) [('A', 0), ('B', 1)]
    --                     , leaf (order 3) [('C', 2), ('D', 3)]
    --                     ])
