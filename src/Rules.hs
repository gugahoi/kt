module Rules where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Data.List

import Configuration (KtConfiguration, ktConfigurationComponent, ktConfigurationEnvironment)
import File

buildRules' :: KtConfiguration -> Rules ()
buildRules' conf = do
    let env = ktConfigurationEnvironment conf
    let comp = ktConfigurationComponent conf

    -- PHONIES
    phony "compile" $ do
        let fullTemplatePath = templateComponentPath comp
        templateFiles <- getDirectoryFiles fullTemplatePath ["//*.yaml"]
        need [buildFolderFor conf </> "compiled" </> fullTemplatePath </> f | f <- templateFiles]

    phony "inject" $ do
        let fullTemplatePath = templateComponentPath comp
        templateFiles <- getDirectoryFiles fullTemplatePath ["//*.yaml"]
        need [buildFolderFor conf </> "injected" </> fullTemplatePath </> f | f <- templateFiles]

    phony "join" $ do
        need [buildFolderFor conf </> "joined.yaml"]

    phony "validate" $ do
        need [buildFolderFor conf </> "validated.txt"]

    phony "deploy" $ do
        need [buildFolderFor conf </> "deployed.txt"]

    phony "delete" $ do
        need [buildFolderFor conf </> "deleted.txt"]

    -- RULES
    "_build" <//> "compiled" <//> "*.yaml" %> \out -> do
        let input = templateFolder </> (stripFrontDirs 5 out)
        let fullEnv = envFileFor conf
        need [input, fullEnv]
        cmd_ "gomplate" "--out" out "--file" input ("--datasource config=" ++ fullEnv)

    "_build" <//> "joined.yaml" %> \out -> do
        let fullTemplatePath = templateComponentPath comp
        files <- getDirectoryFiles fullTemplatePath ["//*.yaml"]
        let compiledFiles = sort [buildFolderFor conf </> "injected" </> fullTemplatePath </> f | f <- files]
        need compiledFiles
        cmd_ Shell "cat" compiledFiles ">" out

    "_build" <//> "injected" <//> "*.yaml" %> \out -> do
        let input = buildFolderFor conf </> "compiled" </> (stripFrontDirs 4 out)
        need [input]
        cmd_ "gomplate" "--out" out "--file" metaTemplate ("--datasource template=" ++ input)

    "_build" <//> "deployed.txt" %> \out -> do
        let joined = buildFolderFor conf </> "joined.yaml"
        need [joined]
        Stdout s <- cmd [FileStdout out] "kubectl" "apply" "-f" joined
        putNormal s

    "_build" <//> "deleted.txt" %> \out -> do
        let joined = buildFolderFor conf </> "joined.yaml"
        need [joined]
        Stdout s <- cmd [FileStdout out] "kubectl" "delete" "-f" joined
        putNormal s

    "_build" <//> "validated.txt" %> \out -> do
        let joined = buildFolderFor conf </> "joined.yaml"
        need [joined]
        Stdout s <- cmd [FileStdout out] "kubectl" "apply" "--validate" "--dry-run" "-f" joined
        putNormal s
