module Main where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Data.Maybe
import Data.List (find)

import Flags
import Rules

buildRules :: [KubeCmdFlags] -> [String] -> IO (Maybe (Rules ()))
buildRules flags targets = return $ Just $ do
    if null targets then want [] else want targets

    phony "clean" $ do
      putNormal "Cleaning files in _build..."
      removeFilesAfter "_build" ["//*"]

    buildRules' flags

buildRules' :: [KubeCmdFlags] -> Rules ()
buildRules' [] = return ()
buildRules' flags = do
    let pathCreator = parseFlags flags
    return ()

main :: IO ()
main = shakeArgsWith shakeOptions{shakeThreads=4, shakeFiles="_build"} flags $ buildRules
