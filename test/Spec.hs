import Test.Hspec

import Rosby.Protocol.Command
import Rosby.Protocol.Serial

main :: IO ()
main = hspec $ do
  describe "fromPrim" $ do
    it "should parse a SET command" $ do
      let cmd = Array 3 [Str 3 "SET", Str 3 "FOO", Str 3 "BAR"]
      fromPrim cmd `shouldBe` (Right $ Set (Key "FOO") "BAR")

    it "should parse a GET command" $ do
      let cmd = Array 2 [Str 3 "GET", Str 3 "FOO"]
      fromPrim cmd `shouldBe` (Right $ Get (Key "FOO"))

    it "should parse a DEL command" $ do
      let cmd = Array 2 [Str 3 "DEL", Str 3 "FOO"]
      fromPrim cmd `shouldBe` (Right $ Delete (Key "FOO"))

    it "should be an array" $ do
      let cmd = Str 3 "FOO"
      fromPrim cmd `shouldBe` Left "Invalid command"

    it "should have a valid str as the first element in an array" $ do
      let cmd = Array 1 [Str 3 "LSKDJFLSKDJFLSKDJFLKSJDLKFJ"]
      fromPrim cmd `shouldBe` Left "Unrecognized command"
