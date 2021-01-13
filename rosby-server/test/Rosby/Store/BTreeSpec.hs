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
        (unzipper . zipper $ leaf (order 3) $ M.fromList [('A', 0)])
          `shouldBe`
          (leaf (order 3) $ M.fromList [('A', 0)])
      it "moving down then back to top returns the root" $ do
        let root = node (order 3) (V.fromList ['C'])
              $ V.fromList
              [ leaf (order 3) $ M.fromList [('A', 0), ('B', 1)]
              , leaf (order 3) $ M.fromList [('C', 2), ('D', 3)]
              ]
        unzipper (fromMaybe undefined (moveDown 0 $ zipper root))
          `shouldBe`
          root

      it "should move down, up, then unzip to the root" $ do
        let root = node (order 3) (V.fromList ['C'])
              $ V.fromList
              [ leaf (order 3) $ M.fromList [('A', 0), ('B', 1)]
              , leaf (order 3) $ M.fromList [('C', 2), ('D', 3)]
              ]
        unzipper (fromMaybe undefined (moveUp <=< moveDown 0 $ zipper root))
          `shouldBe`
          root

    describe "insert" $ do
      it "should keep a value inserted into an empty root" $ do
        let root = leaf (order 3) M.empty
        insert 'A' 0 root
          `shouldBe`
          (leaf (order 3) $ M.fromList [('A', 0)])

      it "should split a root leaf into a node" $ do
        let root = leaf (order 3) $ M.fromList [('A', 0), ('B', 1), ('C', 2)]
        insert 'D' 3 root
        `shouldBe`
          (node (order 3) (V.fromList ['C'])
           $ V.fromList [ leaf (order 3) $ M.fromList [('A', 0), ('B', 1)]
                        , leaf (order 3) $ M.fromList [('C', 2), ('D', 3)]
                        ])
