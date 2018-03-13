module File where

import Development.Shake.FilePath
import Development.Shake

import Configuration

type Component = Maybe String

metaTemplate :: FilePath
metaTemplate = "/opt" </> "meta" </> "release.yaml"

buildFolder :: FilePath
buildFolder = "_build"

templateFolder :: FilePath
templateFolder = "templates"

envFolder :: FilePath
envFolder = "envs"

templateComponentPath :: Component -> FilePath
templateComponentPath = maybe templateFolder ((</>) templateFolder)

buildFolderFor :: KtConfiguration -> FilePath
buildFolderFor (KtConfiguration env Nothing) = buildFolder </> env </> "_all"
buildFolderFor (KtConfiguration env (Just comp)) = buildFolder </> env </> comp

templateFolderFor :: KtConfiguration -> FilePath
templateFolderFor (KtConfiguration _ Nothing) = templateFolder
templateFolderFor (KtConfiguration _ (Just comp)) = templateFolder </> comp

envFileFor :: KtConfiguration -> FilePath
envFileFor (KtConfiguration env _) = envFolder </> env <.> "yaml"

stripFrontDirs :: Int -> FilePath -> FilePath
stripFrontDirs dirLevel = joinPath . drop dirLevel . splitPath
