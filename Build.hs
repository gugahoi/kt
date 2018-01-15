import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

envFromBuild :: FilePath -> String
envFromBuild pth = (splitDirectories pth !! 1)

aliasPhonyWithFiles :: String -> Action ()
aliasPhonyWithFiles folder = do
  env <- getEnvWithDefault "dev" "env"
  files <- getDirectoryFiles "templates" ["//*.yaml"]
  need ["_build" </> env </> folder </> f | f <- files]

aliasPhony :: String -> Action ()
aliasPhony file = do
  env <- getEnvWithDefault "dev" "env"
  need ["_build" </> env </> file]

main :: IO ()
main = shakeArgs shakeOptions{shakeThreads=4, shakeFiles="_build"} $ do

  phony "clean" $ do
    putNormal "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  phony "compile" $ aliasPhonyWithFiles "compiled"

  phony "inject" $ aliasPhonyWithFiles "injected"

  phony "join" $ aliasPhony "joined.yaml"
    
  phony "deploy" $ aliasPhony "deployed.txt"
  
  phony "validate" $ aliasPhony "validated.txt"

  "_build//joined.yaml" %> \out -> do
    let dir = "_build" </> envFromBuild out </> "injected"
    files <- getDirectoryFiles "templates" ["//*.yaml"]
    let compiled = [dir </> takeFileName f | f <- files]
    need compiled
    putNormal "Joining compiled yaml..."
    cmd_ Shell "cat" compiled ">" out

  "_build//compiled/*.yaml" %> \out -> do
    let input = "templates" </> (takeFileName out)
    let env = "envs" </> (splitDirectories out !! 1) <.> "yaml"
    need [input, env]
    putNormal "Compiling gomplates..."
    cmd_ "gomplate" "--out" out "--file" input ("--datasource config=" ++ env)

  "_build//injected/*.yaml" %> \out -> do
    let input = "_build" </> envFromBuild out </> "compiled" </> (takeFileName out)
    let meta = "meta" </> "release.yaml"
    need [input, meta]
    putNormal "Compiling meta gomplate..."
    cmd_ Shell "echo" [input] "|" "gomplate" "--out" out "--file" meta ("--datasource template=" ++ input) "--datasource name=stdin:"

  "_build//deployed.txt" %> \out -> do
    let join = "_build" </> envFromBuild out </> "joined.yaml"
    need [join]
    cmd_ Shell "kubectl" "apply" "-f" join "|" "tee" out

  "_build//validated.txt" %> \out -> do
    let join = "_build" </> envFromBuild out </> "joined.yaml"
    need [join]
    cmd_ Shell "kubectl" "apply" "--validate" "--dry-run" "-f" join "|" "tee" out
