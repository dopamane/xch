module Main (main) where

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Test.Xch" []
