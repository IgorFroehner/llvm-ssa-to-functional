
module Main (main) where

import Test.Hspec (hspec)
import qualified Spec

main :: IO ()
main = do
    hspec Spec.spec
