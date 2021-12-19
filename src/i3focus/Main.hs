{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment        (getArgs)
import System.Exit               (ExitCode(ExitSuccess))
import System.Process            (readProcessWithExitCode)
import Data.Vector               (Vector, (!?), lastM)
import Control.Monad             ((>=>), join)
import Control.Arrow             ((>>>))
import Data.Scientific           (Scientific, formatScientific, FPFormat (Fixed))
import Lib (parseJSON', runCmd, Window, currentWorkspace, parseID, getAllWorkspaces, getWorkspaceByID, getWindows)


i3Msg :: String -> IO (ExitCode, String, String)
i3Msg type_ = readProcessWithExitCode "i3-msg" ["-t", type_] []

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      -- 1 is the leftmost key, not 0
      let idx = (read x :: Int) - 1
      (code_w, stdout_w, stderr_w) <- i3Msg "get_workspaces"
      (code_t, stdout_t, stderr_t) <- i3Msg "get_tree"
      case (code_w, code_t) of
        (ExitSuccess, ExitSuccess) -> currentWorkspace tryParse idx stdout_w stdout_t
        _                          -> putStrLn stderr_w >> putStrLn stderr_t
    _ -> pure ()

tryParse :: Int -> Scientific -> String -> IO ()
tryParse idx workspace =
  (parseJSON'
  >=> getAllWorkspaces
  >=> getWorkspaceByID workspace
  >=> getWindows
  >=> getWindowIds
  >>> generateCmd idx)
  >>> runCmd

getWindowIds :: Vector Window -> Vector (Maybe Scientific)
getWindowIds windows = parseID <$> windows

generateCmd :: Int -> Vector (Maybe Scientific) -> Maybe String
generateCmd (-1) ids = mkMessage <$> join (lastM ids)
generateCmd idx  ids = mkMessage <$> join (ids !? idx)

mkMessage :: Scientific -> String
mkMessage c = "i3-msg \"[con_id=" <> scientificToString c <> "] focus\""

scientificToString :: Scientific -> String
scientificToString = formatScientific Fixed (Just 0)

