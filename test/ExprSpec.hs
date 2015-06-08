module ExprSpec where

import           Expr
import           Parser
import           Test.Hspec

spec :: Spec
spec = do
  describe "parseOp" $ do
    it "parses operators" $ do
      runParser parseOp ">++" `shouldBe` Just (Forward, "++")
  describe "parseExpr" $ do
    it "parses expression" $ do
      runParser parseExpr ">++" `shouldBe` Just (Comb [A Forward, A Inc, A Inc], "")
