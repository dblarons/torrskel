import Bencode
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Encoding" $ do
    describe "Integer encoding" $ do
      it "should encode integers" $ do
        let exp = "i3e"
        encode (BInteger 3) `shouldBe` exp
      it "should encode negative integers" $ do
        let exp = "i-3e"
        encode (BInteger (-3)) `shouldBe` exp
      it "should not pad integers with 0s" $ do
        let exp = "i3e"
        encode (BInteger 0003) `shouldBe` exp
  
    describe "String encoding" $ do
      it "should encode strings" $ do
        let exp = "4:spam"
        encode (BString "spam") `shouldBe` exp
      it "should encode the empty string" $ do
        let exp = "0:"
        encode (BString "") `shouldBe` exp
  
    describe "List encoding" $ do
      it "should encode lists of one type" $ do
        let exp = "li3ei4ei5ee"
        encode (BList [BInteger 3, BInteger 4, BInteger 5]) `shouldBe` exp
      it "should encode lists of mixed type" $ do
        let exp = "l4:spami3e4:eggse"
        encode (BList [BString "spam", BInteger 3, BString "eggs"]) `shouldBe` exp
      it "should encode an empty list" $ do
        let exp = "le"
        encode (BList []) `shouldBe` exp
      it "should encode a list of lists" $ do
        let exp = "l4:spamli3ei4eee"
        encode (BList [BString "spam", BList [BInteger 3, BInteger 4]]) `shouldBe` exp
      it "should encode a list of dictionaries" $ do
        let exp = "ld3:cow3:mooed3:cow3:mooee"
        encode (BList [BDict [("cow", BString "moo")], 
                      BDict [("cow", BString "moo")]]) `shouldBe` exp
  
    describe "Dictionary encoding" $ do
      it "should encode a dictionary with an integer value" $ do
        let exp = "d3:fooi3ee"
        encode (BDict [("foo", BInteger 3)]) `shouldBe` exp
      it "should encode a dictionary with a string value" $ do
        let exp = "d3:foo3:bare"
        encode (BDict [("foo", BString "bar")]) `shouldBe` exp
      it "should encode a dictionary with a list value" $ do
        let exp = "d3:fooli3eee"
        encode (BDict [("foo", BList [BInteger 3])]) `shouldBe` exp
      it "should encode a dictionary with a dictionary value" $ do
        let exp = "d3:food3:bari3eee"
        encode (BDict [("foo", BDict [("bar", BInteger 3)])]) `shouldBe` exp
      it "should encode an empty dictionary" $ do
        let exp = "de"
        encode (BDict []) `shouldBe` exp

  describe "Decoding" $ do
    describe "Integer decoding" $ do
      it "should decode an integer" $ do
        let exp = BInteger 3
        decode "i3e" `shouldBe` exp
      

