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

    buildRules' flags

buildRules' :: [KubeCmdFlags] -> Rules ()
buildRules' [] = return ()
buildRules' flags = do
    let env = case find envFinder flags of
                Just (KubeEnvironment env) -> env
                Nothing -> "dev"
    let comp = case find compFinder flags of
                 Just (KubeComponent comp) -> Just comp
                 Nothing -> Nothing

    compilePhony env comp
    joinPhony env
    validatePhony env
    deployPhony env
    deletePhony env

    compileRule env
    joinRule env comp
    injectRule env
    validateRule env
    deployRule env
    deleteRule env
  where
    envFinder (KubeEnvironment _) = True
    envFinder _ = False

    compFinder (KubeComponent _) = True
    compFinder _ = False

main :: IO ()
main = shakeArgsWith shakeOptions{shakeThreads=4, shakeFiles="_build"} cliFlags $ buildRules
