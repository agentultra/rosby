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

      it "should be able to merge a node to a parent" $ do
        let lf = node (order 3) (V.fromList ['D', 'E'])
                 $ V.fromList
                 [ leaf (order 3) [('A', 0), ('B', 1), ('C', 2)]
                 , leaf (order 3) [('D', 3)]
                 , leaf (order 3) [('E', 4), ('F', 5), ('G', 6)]
                 ]
            splitNode = node (order 3) (V.singleton 'G')
              $ V.fromList
              [ leaf (order 3) [('E', 4), ('F', 5)]
              , leaf (order 3) [('G', 6), ('H', 7)]
              ]
        (unzipper (fromMaybe undefined (mergeUp splitNode <=< moveDown 2 $ zipper lf)))
          `shouldBe`
          (node (order 3) (V.fromList ['D', 'E', 'G'])
           $ V.fromList
           [ leaf (order 3) [('A', 0), ('B', 1), ('C', 2)]
           , leaf (order 3) [('D', 3)]
           , leaf (order 3) [('E', 4), ('F', 5)]
           , leaf (order 3) [('G', 6), ('H', 7)]
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

      describe "insertFind" $ do
        it "should focus the zipper on the right most node" $ do
          let root = node (order 3) (V.fromList ['D', 'E'])
                $ V.fromList
                [ leaf (order 3) [('A', 0), ('B', 1), ('C', 2)]
                , leaf (order 3) [('D', 3)]
                , leaf (order 3) [('E', 4), ('F', 5), ('G', 6)]
                ]
          (find 'H' $ zipper root)
            `shouldBe`
            (Just (leaf (order 3) [('E', 4), ('F', 5), ('G', 6)],
             [ DownTo 2 (order 3) (V.fromList ['D', 'E']) $ V.fromList
                 [ leaf (order 3) [('A', 0), ('B', 1), ('C', 2)]
                 , leaf (order 3) [('D', 3)]
                 , leaf (order 3) [('E', 4), ('F', 5), ('G', 6)]
                 ]
             ]))

        it "should focus the zipper on the left most node" $ do
          let root = node (order 3) (V.fromList ['D', 'E'])
                $ V.fromList
                [ leaf (order 3) [('A', 0), ('B', 1), ('C', 2)]
                , leaf (order 3) [('D', 3)]
                , leaf (order 3) [('E', 4), ('F', 5), ('G', 6)]
                ]
          (find 'A' $ zipper root)
            `shouldBe`
            (Just (leaf (order 3) [('A', 0), ('B', 1), ('C', 2)],
             [ DownTo 0 (order 3) (V.fromList ['D', 'E']) $ V.fromList
                 [ leaf (order 3) [('A', 0), ('B', 1), ('C', 2)]
                 , leaf (order 3) [('D', 3)]
                 , leaf (order 3) [('E', 4), ('F', 5), ('G', 6)]
                 ]
             ]))

        it "should focus the zipper on towards the right" $ do
          let root = node (order 3) (V.fromList ['D', 'E'])
                $ V.fromList
                [ leaf (order 3) [('A', 0), ('B', 1), ('C', 2)]
                , leaf (order 3) [('D', 3)]
                , leaf (order 3) [('E', 4), ('F', 5), ('G', 6)]
                ]
          (find 'D' $ zipper root)
            `shouldBe`
            (Just (leaf (order 3) [('D', 3)],
             [ DownTo 1 (order 3) (V.fromList ['D', 'E']) $ V.fromList
                 [ leaf (order 3) [('A', 0), ('B', 1), ('C', 2)]
                 , leaf (order 3) [('D', 3)]
                 , leaf (order 3) [('E', 4), ('F', 5), ('G', 6)]
                 ]
             ]))

      describe "split" $ do
        it "should split a leaf" $ do
          let root = leaf (order 3) [('A', 0), ('B', 1), ('C', 2)]
          split' 'D' 3 root
            `shouldBe`
            Just $ node (order 3) (V.singleton 'C') $ V.fromList [leaf (order 3) [('A', 0), ('B', 1)], leaf (order 3) [('C', 2), ('D', 3)]]

      -- describe "insertWithMerge" $ do
      --   it "should insert the value in an empty root/leaf" $ do
      --     let root = leaf (order 3) []
      --     (fmap unzipper <$> insertWithMerge 'A' 0 . zipper $ root)
      --       `shouldBe`
      --       (Just $ leaf (order 3) [('A', 0)])

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
