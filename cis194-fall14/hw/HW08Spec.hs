import Test.Hspec

import HW08
import HW08_Risk


main = hspec $ do
    describe "stringFitsFormat" $ do
        it "detects whether or not the input string fits the format" $ do
            stringFitsFormat "" `shouldBe` False
            stringFitsFormat "0" `shouldBe` True
            stringFitsFormat "1" `shouldBe` False
            stringFitsFormat "a" `shouldBe` False
            stringFitsFormat "1a" `shouldBe` True
            stringFitsFormat "1b" `shouldBe` False
            stringFitsFormat "9aaaaaaaaa" `shouldBe` True
            stringFitsFormat "10aaaaaaaaaa" `shouldBe` False
            stringFitsFormat "001a" `shouldBe` True
            stringFitsFormat "2aa3aaa" `shouldBe` True
            stringFitsFormat "2aa3aa" `shouldBe` False
            stringFitsFormat "2bb2bb" `shouldBe` False

    describe "battleResults" $ do
        it "returns the change in ArmyCounts" $ do
            battleResults [3,6,4] [5,5] `shouldBe` ArmyCounts { attackers = -1, defenders = -1 }
            battleResults [3,6,4] [5,6] `shouldBe` ArmyCounts { attackers = -2, defenders = 0 }
            battleResults [4] [3,2] `shouldBe` ArmyCounts { attackers = 0, defenders = -1 }