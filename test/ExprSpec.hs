module ExprSpec where

import           Expr
import           Parser
import           Test.Hspec

spec :: Spec
spec = do
  describe "parseOp" $ do
    it "parses operators" $ do
      runParser parseForward ">++" `shouldBe` Just (Forward 1, "++")
      runParser parseInc "++>" `shouldBe` Just (Inc 2, ">")
  describe "parseExpr" $ do
    it "parses expression" $ do
       runParser parseExpr ">++" `shouldBe` Just ([Forward 1, Inc 2], "")
       runParser parseExpr "> ++" `shouldBe` Just ([Forward 1, Inc 2], "")
       runParser parseExpr ">[>,<[<.]]" `shouldBe` Just ([Forward 1,Loop [Forward 1,Read 1,Backward 1,Loop [Backward 1,Put 1]]], "")
       runParser parseExpr "[ [[-]>]>]" `shouldBe` Just ([Loop [Loop [Loop [Dec 1],Forward 1],Forward 1]],"")
    it "parses hanoi.b" $ do
       runParser parseExpr "\n[\n>[-]>+]\n" `shouldBe` Just ([Loop [Forward 1,Loop [Dec 1],Forward 1,Inc 1]],"")
  describe "comments" $ do
    it "parses anything other than reserved operators" $ do
      runParser comments "hello\n.world" `shouldBe` Just ("hello\n", ".world")
      runParser comments "hello + world" `shouldBe` Just ("hello ", "+ world")
