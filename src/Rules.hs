module Rules where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Data.List

import Configuration (KtConfiguration, ktConfigurationComponent, ktConfigurationEnvironment)

type Environment = String
type Component = Maybe String

-- PATH MANIPULATIONS
metaTemplate :: FilePath
metaTemplate = "/opt" </> "meta" </> "release.yaml"

envFolder :: FilePath
envFolder = "envs"

templateFolder :: FilePath
templateFolder = "templates"

templateComponentPath :: Component -> FilePath
templateComponentPath = maybe templateFolder ((</>) templateFolder)

buildPathWithComponentTemplate :: FilePath -> FilePath
buildPathWithComponentTemplate = dropDirectory1 . dropDirectory1 . dropDirectory1

stripFrontDirs :: Int -> FilePath -> FilePath
stripFrontDirs dirLevel = joinPath . drop dirLevel . splitPath

-- RULES
compileRule :: Environment -> Rules ()
compileRule env = "_build" </> env </> "compiled" <//> "*.yaml" %> \out -> do
    let input = templateFolder </> (stripFrontDirs 4 out)
    let fullEnv = envFolder </> env <.> "yaml"
    need [input, fullEnv]
    cmd_ "gomplate" "--out" out "--file" input ("--datasource config=" ++ fullEnv)

joinRule :: Environment -> Component -> Rules ()
joinRule env comp = "_build" </> env </> "joined.yaml" %> \out -> do
    let fullTemplatePath = templateComponentPath comp
    files <- getDirectoryFiles fullTemplatePath ["//*.yaml"]
    let compiledFiles = sort ["_build" </> env </> "injected" </> fullTemplatePath </> f | f <- files]
    need compiledFiles
    cmd_ Shell "cat" compiledFiles ">" out

injectRule :: Environment -> Rules ()
injectRule env = "_build" </> env </> "injected" <//> "*.yaml" %> \out -> do
    let input = "_build" </> env </> "compiled" </> (buildPathWithComponentTemplate out)
    need [input]
    cmd_ "gomplate" "--out" out "--file" metaTemplate ("--datasource template=" ++ input)

deployRule :: Environment -> Rules ()
deployRule env = "_build" </> env </> "deployed.txt" %> \out -> do
    let joined = "_build" </> env </> "joined.yaml"
    need [joined]
    Stdout s <- cmd [FileStdout out] "kubectl" "apply" "-f" joined
    putNormal s

deleteRule :: Environment -> Rules ()
deleteRule env = "_build" </> env </> "deleted.txt" %> \out -> do
    let joined = "_build" </> env </> "joined.yaml"
    need [joined]
    Stdout s <- cmd [FileStdout out] "kubectl" "delete" "-f" joined
    putNormal s

validateRule :: Environment -> Rules ()
validateRule env = "_build" </> env </> "validated.txt" %> \out -> do
    let joined = "_build" </> env </> "joined.yaml"
    need [joined]
    Stdout s <- cmd [FileStdout out] "kubectl" "apply" "--validate" "--dry-run" "-f" joined
    putNormal s

buildRules' :: KtConfiguration -> Rules ()
buildRules' conf = do
    let env = ktConfigurationEnvironment conf
    let comp = ktConfigurationComponent conf

    -- PHONIES
    phony "compile" $ do
        let fullTemplatePath = templateComponentPath comp
        templateFiles <- getDirectoryFiles fullTemplatePath ["//*.yaml"]
        need ["_build" </> env </> "compiled" </> fullTemplatePath </> f | f <- templateFiles]

    phony "inject" $ do
        let fullTemplatePath = templateComponentPath comp
        templateFiles <- getDirectoryFiles fullTemplatePath ["//*.yaml"]
        need ["_build" </> env </> "injected" </> fullTemplatePath </> f | f <- templateFiles]

    phony "join" $ do
        need ["_build" </> env </> "joined.yaml"]

    phony "validate" $ do
        need ["_build" </> env </> "validated.txt"]

    phony "deploy" $ do
        need ["_build" </> env </> "deployed.txt"]

    phony "delete" $ do
        need ["_build" </> env </> "deleted.txt"]

    compileRule env
    joinRule env comp
    injectRule env
    validateRule env
    deployRule env
    deleteRule env
