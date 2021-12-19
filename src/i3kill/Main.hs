{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit               (ExitCode(ExitSuccess, ExitFailure))
import System.Process            (readProcessWithExitCode)
import Data.Aeson                (Value(Bool, Array))
import Data.Aeson.Lens           (key, nth)
import Control.Lens              ((^?))
import Data.Vector               (Vector, (!?))
import Control.Monad             ((>=>))
import Control.Arrow             ((>>>))
import qualified Data.Vector as V
import Lib (parseJSON', runCmd, Window, Workspace, enumerate, getWindows)

main :: IO ()
main = do
  (code, stdout, stderr) <- readProcessWithExitCode "i3-msg" ["-t", "get_tree"] []
  case code of
    ExitFailure _ -> putStrLn stderr
    ExitSuccess   -> tryParse stdout

-- 1. Parse the tree (Value)
-- 2. If successful, get workspaces (Vector Workspace)
-- 3. If successful, for each workspace, get the windows (Vector (Vector Window))
--   4. If successful, enumerate windows (Vector (Vector (Int, Window)))
--   5. Get the first focused window (Vector (Maybe (Int, Window)))
--   6. If list non-empty, generate command from index (Vector (Maybe String))
-- 7. End for each
-- 8. Get the first successful command (String)
-- 9. Run the command
tryParse :: String -> IO ()
tryParse =
  (parseJSON'
  >=> getWorkspaces
  >=> fmap (getWindows >=> enumerate >>> getFirstFocused >>> fmap generateCmd)
  >>> getFirstJust)
  >>> runCmd

getWorkspaces :: Value -> Maybe (Vector Workspace)
getWorkspaces json = do
  case json ^? key "nodes" . nth 1 . key "nodes" . nth 1 . key "nodes" of
    Just (Array x) -> pure x
    _              -> Nothing

-- Assumes invalid JSON is not focused
filterFocused :: Vector (Int, Window) -> Vector (Int, Window)
filterFocused = V.filter (\(_, window) ->
    case window ^? key "focused" of
      Just (Bool x) -> x
      _             -> False
  )

getFirstFocused :: Vector (Int, Window) -> Maybe (Int, Window)
getFirstFocused = filterFocused >>> (!? 0)

getFirstJust :: Vector (Maybe a) -> Maybe a
getFirstJust = V.catMaybes >>> (!? 0)

generateCmd :: (Int, a) -> String
generateCmd (0, _) = "i3-msg kill"
generateCmd (_, _) = "i3-msg 'kill; focus left'"

