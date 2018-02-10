module ConfigurationSpec where

import Test.Hspec
import Configuration

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- -- not needed for automatic spec discovery.
-- main :: IO ()
-- main = hspec spec
--

spec :: Spec
spec = do
  describe "parseFlags" $ do
      it "converts the list of cmd flags to a config object" $ do
          parseFlags [KubeEnvironment "env1", KubeComponent ""] == (KtConfiguration "env1" Nothing)

      it "converts the list of cmd flags to a config object with a component" $ do
          parseFlags [KubeEnvironment "env1", KubeComponent "comp"] == (KtConfiguration "env1" (Just "comp"))

  describe "stripFrontDirs" $ do
      it "drops the dirs to the level you specify" $ do
          stripFrontDirs 2 "path/to/the/file.yaml" == "the/file.yaml"

      it "drops nothing with zero depth" $ do
          stripFrontDirs 0 "path/to/the/file.yaml" == "path/to/the/file.yaml"

      it "returns empty string when depth is too big" $ do
          stripFrontDirs 10 "path/to/the/file.yaml" == ""
