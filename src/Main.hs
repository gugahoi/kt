module Main where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import System.Console.GetOpt

import Data.Maybe
import Data.List (find)

import Configuration
import Rules

buildRules :: [KubeCmdFlags] -> [String] -> IO (Maybe (Rules ()))
buildRules flags targets = return $ Just $ do
    if null targets then want [] else want targets

    phony "clean" $ do
      putNormal "Cleaning files in _build..."
      removeFilesAfter "_build" ["//*"]

    buildRules' $ parseFlags flags

main :: IO ()
main = shakeArgsWith shakeOptions{shakeThreads=4, shakeFiles="_build"} cliFlags $ buildRules
