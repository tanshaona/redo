{-# LANGUAGE StandaloneDeriving #-}
-- file: redo.hs
import Debug.Trace (traceShow)
import Data.Maybe (listToMaybe)
import Data.Map.Lazy (insert, fromList, toList, adjust)
import Control.Monad (filterM, liftM)
import System.Exit (ExitCode(..))
import System.Environment (getArgs, getEnvironment)
import System.IO (hPutStrLn, stderr)
import System.FilePath (hasExtension, replaceBaseName, takeBaseName)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..), StdStream(..), CmdSpec(..))
import System.Directory (removeFile, renameFile, doesFileExist) 


traceShow' arg = traceShow arg arg
--trace' arg = trace (show arg) arg

deriving instance Show CreateProcess

deriving instance Show StdStream

deriving instance Show CmdSpec



main :: IO ()
main = mapM_ redo =<< getArgs
--main = getArgs >>= mapM_ redo
{-
main = do
    args <- getArgs
    mapM_ redo args
-}


redo :: String -> IO ()
redo target = maybe printMissing redo' =<< redoPath target
    where redo' :: FilePath -> IO ()
          redo' path = do
            oldEnv <- getEnvironment
            let newEnv = toList $ adjust (++ ":./") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
            (_,_,_,ph) <- createProcess $ traceShow' $ (shell $ cmd path) {env = Just newEnv}
            exit <- waitForProcess ph
            case traceShow' exit of
                ExitSuccess -> do renameFile tmp target
                ExitFailure code -> do hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
                                       removeFile tmp
          tmp = target ++ "----redoing"
          printMissing = error $ "No .do file found for target '" ++ target ++ "'"
          cmd path = traceShow' $ unwords ["zsh", path, "0", takeBaseName target, tmp]
--          cmd path = unwords ["zsh", path, "0", takeBaseName target, tmp, ">", tmp]

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = --liftM safeHead $ filterM doesFileExist candidates
    listToMaybe `liftM` filterM doesFileExist candidates
    where candidates = [target ++ ".do"] ++ if hasExtension target 
                                            then [replaceBaseName target "default" ++ ".do"] 
                                            else []
