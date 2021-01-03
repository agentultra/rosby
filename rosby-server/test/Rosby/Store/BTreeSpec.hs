module Rosby.Store.BTreeSpec where

import Test.Hspec
import qualified Data.Map.Strict as M

import Rosby.Store.BTree

spec :: Spec
spec = do
  context "Rosby.Store.BTree" $ do
    describe "insert" $ do
      it "should keep a value inserted into an empty root" $ do
        let root = leaf (order 3) M.empty
        insert 'A' 0 root
          `shouldBe`
          (leaf (order 3) $ M.fromList [('A', 0)])
