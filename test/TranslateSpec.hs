
module TranslateSpec (spec) where

import Test.Hspec

import qualified Ast
import qualified Anf
import Translate (translate)

spec :: Spec
spec = parallel $ do
  describe "translate" $ do
    it "translates correctly a empty program" $ do
      let ast = Ast.Program []
      translate ast `shouldBe` Anf.Program []
