import Bencode
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Integer encoding and decoding" $ do
    it "should encode and decode integers" $ do
      let expected = "i3e"
      let bin = BInteger 3
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode negative integers" $ do
      let expected = "i-3e"
          bin = BInteger (-3)
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should not pad integers with 0s" $ do
      let expected = "i3e"
          bin = BInteger 0003
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
  
  describe "String encoding and decoding" $ do
    it "should encode and decode strings" $ do
      let expected = "4:spam"
          bin = BString "spam"
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode the empty string" $ do
      let expected = "0:"
          bin = BString ""
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
  
  describe "List encoding and decoding" $ do
    it "should encode and decode lists of one type" $ do
      let expected = "li3ei4ei5ee"
          bin = BList [BInteger 3, BInteger 4, BInteger 5]
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode lists of mixed type" $ do
      let expected = "l4:spami3e4:eggse"
          bin = BList [BString "spam", BInteger 3, BString "eggs"]
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode an empty list" $ do
      let expected = "le"
          bin = BList []
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode a list of lists" $ do
      let expected = "l4:spamli3ei4eee"
          bin = BList [BString "spam", BList [BInteger 3, BInteger 4]]
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode a list of dictionaries" $ do
      let expected = "ld3:cow3:mooed3:cow3:mooee"
          bin = BList [BDict [("cow", BString "moo")], 
                        BDict [("cow", BString "moo")]]
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
  
  describe "Dictionary encoding and decoding" $ do
    it "should encode and decode a dictionary with an integer value" $ do
      let expected = "d3:fooi3ee"
          bin = BDict [("foo", BInteger 3)]
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode a dictionary with a string value" $ do
      let expected = "d3:foo3:bare"
          bin = BDict [("foo", BString "bar")]
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode a dictionary with a list value" $ do
      let expected = "d3:fooli3eee"
          bin = BDict [("foo", BList [BInteger 3])]
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode a dictionary with a dictionary value" $ do
      let expected = "d3:food3:bari3eee"
          bin = BDict [("foo", BDict [("bar", BInteger 3)])]
      encode bin `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode an empty dictionary" $ do
      let expected = "de"
          bin = BDict []
      encode bin `shouldBe` expected      
      decode expected `shouldBe` Right bin

