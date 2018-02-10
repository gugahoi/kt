module Rules where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Data.List

import Configuration


buildRules' :: [KubeCmdFlags] -> Rules ()
buildRules' [] = return ()
buildRules' flags = do
    let config = parseFlags flags

    buildFolder <//> "compiled" <//> "*.yaml" %> \out -> do
        let envFile = envFileFor config
        let input = templateFolderFor config </> stripFrontDirs 3 out
        templateFiles <- getDirectoryFiles $ templateFolderFor config
        need [input, envFile]

    return ()


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

templateFilesToDatasourceFlags :: FilePath -> [FilePath] -> [String]
templateFilesToDatasourceFlags currentTemplate files = ["--datasource " ++ f ++ "=" ++ templateFolder </> f | f <- files, f /= currentTemplate]

stripFrontDirs :: Int -> FilePath -> FilePath
stripFrontDirs dirLevel = joinPath . drop dirLevel . splitPath

-- RULES

class ExecutableRule a where
    getNeeds :: a -> FilePath -> [FilePath]


compileRule :: Environment -> Rules ()
compileRule env = "_build" </> env </> "compiled" <//> "*.yaml" %> \out -> do
    let input = templateFolder </> (stripFrontDirs 4 out)
    let fullEnv = envFolder </> env <.> "yaml"
    allTemplateFiles <- getDirectoryFiles templateFolder ["//*"]
    need [input, fullEnv]
    cmd_ "gomplate" "--out" out "--file" input ("--datasource config=" ++ fullEnv) $ intercalate " " $ templateFilesToDatasourceFlags input allTemplateFiles

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
    cmd_ Shell "kubectl" "apply" "-f" joined "|" "tee" out

deleteRule :: Environment -> Rules ()
deleteRule env = "_build" </> env </> "deleted.txt" %> \out -> do
    let joined = "_build" </> env </> "joined.yaml"
    need [joined]
    cmd_ Shell "kubectl" "delete" "-f" joined "|" "tee" out

validateRule :: Environment -> Rules ()
validateRule env = "_build" </> env </> "validated.txt" %> \out -> do
    let joined = "_build" </> env </> "joined.yaml"
    need [joined]
    cmd_ Shell "kubectl" "apply" "--validate" "--dry-run" "-f" joined "|" "tee" out

-- PHONIES
compilePhony :: Environment -> Component -> Rules ()
compilePhony env comp = phony "compile" $ do
    let fullTemplatePath = templateComponentPath comp
    templateFiles <- getDirectoryFiles fullTemplatePath ["//*.yaml"]
    need ["_build" </> env </> "compiled" </> fullTemplatePath </> f | f <- templateFiles]

joinPhony :: Environment -> Rules ()
joinPhony env = phony "join" $ do
    need ["_build" </> env </> "joined.yaml"]

validatePhony :: Environment -> Rules ()
validatePhony env = phony "validate" $ do
    need ["_build" </> env </> "validated.txt"]

deployPhony :: Environment -> Rules ()
deployPhony env = phony "deploy" $ do
    need ["_build" </> env </> "deployed.txt"]

deletePhony :: Environment -> Rules ()
deletePhony env = phony "delete" $ do
    need ["_build" </> env </> "deleted.txt"]
