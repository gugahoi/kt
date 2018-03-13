module FileSpec where

import Test.Hspec
import File

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- -- not needed for automatic spec discovery.
-- main :: IO ()
-- main = hspec spec
--

spec :: Spec
spec = do
  describe "stripFrontDirs" $ do
      it "drops the dirs to the level you specify" $ do
          stripFrontDirs 2 "path/to/the/file.yaml" == "the/file.yaml"

      it "drops nothing with zero depth" $ do
          stripFrontDirs 0 "path/to/the/file.yaml" == "path/to/the/file.yaml"

      it "returns empty string when depth is too big" $ do
          stripFrontDirs 10 "path/to/the/file.yaml" == ""
