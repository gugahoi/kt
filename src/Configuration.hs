module Configuration where

import Data.List
import Data.Maybe

import System.Console.GetOpt

import Development.Shake.FilePath
import Development.Shake

data KubeCmdFlags =
    KubeEnvironment String
  | KubeComponent String
  deriving (Show, Eq)

data KtConfiguration = KtConfiguration
    { ktConfigurationEnvironment :: String
    , ktConfigurationComponent :: Maybe String
    } deriving (Eq)

flags :: [OptDescr (Either String KubeCmdFlags)]
flags =
    [ Option "e" ["kt-environment"] (ReqArg parseEnvironment "ENVIRONMENT") "The Kubernetes environment to deploy to (name of file in 'env' folder sans .yaml)."
    , Option "c" ["kt-component"] (ReqArg parseComponent "COMPONENT") "The component (a subfolder under your templates dir) you want to deploy."
    ]

parseEnvironment :: String -> Either String KubeCmdFlags
parseEnvironment = Right . KubeEnvironment

parseComponent :: String -> Either String KubeCmdFlags
parseComponent = Right . KubeComponent

parseFlags :: [KubeCmdFlags] -> KtConfiguration
parseFlags = foldl merger (KtConfiguration "" Nothing)
    where
        merger (KtConfiguration e c) (KubeEnvironment newE) = KtConfiguration newE c
        merger (KtConfiguration e c) (KubeComponent "") = KtConfiguration e Nothing
        merger (KtConfiguration e c) (KubeComponent newC) = KtConfiguration e (Just newC)

buildFolder :: FilePath
buildFolder = "_build"

templateFolder :: FilePath
templateFolder = "templates"

envFolder :: FilePath
envFolder = "envs"

buildFolderFor :: KtConfiguration -> FilePath
buildFolderFor (KtConfiguration env Nothing) = buildFolder </> env </> "all"
buildFolderFor (KtConfiguration env (Just comp)) = buildFolder </> env </> comp

templateFolderFor :: KtConfiguration -> FilePath
templateFolderFor (KtConfiguration _ Nothing) = templateFolder
templateFolderFor (KtConfiguration _ (Just comp)) = templateFolder </> comp

envFileFor :: KtConfiguration -> FilePath
envFileFor (KtConfiguration env _) = envFolder </> env <.> "yaml"

stripFrontDirs :: Int -> FilePath -> FilePath
stripFrontDirs dirLevel = joinPath . drop dirLevel . splitPath
