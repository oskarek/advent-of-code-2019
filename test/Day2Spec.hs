module Day2Spec ( spec ) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Day2.Day2
import           Day2.Types

board = mkBoard [1, 2, 2, 5, 2, 2, 2, 2, 99, 4, 3, 6, 4, 2]
instrs = [BinOpInstr Add 2 2 5, BinOpInstr Mult 2 2 2, Halt]

spec :: Spec
spec = do
  describe "binOpAction" $ do
    prop "treats Add as addition" $
      \x y -> binOpAction Add x y `shouldBe` x + y
    prop "treats Mult as multiplication"
      $ \x y -> binOpAction Mult x y `shouldBe` x * y

  describe "applyInstr" $ do
    it "handles and Add instr" $
      applyInstr board (BinOpInstr Add 3 2 8) `shouldBe`
        Just (mkBoard [1, 2, 2, 5, 2, 2, 2, 2, 7, 4, 3, 6, 4, 2])
    it "handles a Mult instr" $
      applyInstr board (BinOpInstr Mult 6 3 11) `shouldBe`
        Just (mkBoard [1, 2, 2, 5, 2, 2, 2, 2, 99, 4, 3, 10, 4, 2])
    it "handles a Halt instr" $
      applyInstr board Halt `shouldBe` Just board

  describe "applyInput" $
    it "applies noun and verb to correct positions" $
      applyInput (10, 4) board `shouldBe`
        mkBoard [1, 10, 4, 5, 2, 2, 2, 2, 99, 4, 3, 6, 4, 2]

  describe "runUntilEndState" $
    it "handles the dummy board" $
      runUntilEndState (2, 3) board instrs `shouldBe`
        Just (mkBoard [1, 2, 9, 5, 2, 6, 2, 2, 99, 4, 3, 6, 4, 2])

  describe "runProgram" $
    it "handles the dummy board" $
      runProgram (2, 3) board instrs `shouldBe` Just 1