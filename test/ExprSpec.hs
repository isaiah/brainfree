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
      runParser parseExpr ">++" `shouldBe` Just ([A Forward, A Inc, A Inc], "")
      runParser parseExpr "> ++" `shouldBe` Just ([A Forward, A Inc, A Inc], "")
      runParser parseExpr ">[>,<[<.]]" `shouldBe` Just ([A Forward,Loop [A Forward,A Read,A Backward,Loop [A Backward,A Put]]], "")
      runParser parseExpr "[ [[-]>]>]" `shouldBe` Just ([Loop [Loop [Loop [A Dec],A Forward],A Forward]],"")
    it "parses hanoi.b" $ do
      runParser parseExpr "\n[\n>[-]>+]\n" `shouldBe` Just ([Loop [A Forward,Loop [A Dec],A Forward,A Inc]],"")
  describe "comments" $ do
    it "parses anything other than reserved operators" $ do
      runParser comments "hello\n.world" `shouldBe` Just ("hello\n", ".world")
      runParser comments "hello + world" `shouldBe` Just ("hello ", "+ world")
