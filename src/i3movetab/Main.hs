{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding            (Right, Left)
import System.Environment        (getArgs)
import System.Exit               (ExitCode(ExitSuccess))
import System.Process            (readProcessWithExitCode)
import Data.Aeson                (Value(Bool))
import Data.Aeson.Lens           (key)
import Control.Lens              ((^?))
import Data.Vector               (Vector, (!?))
import Control.Monad             ((>=>), join)
import Control.Arrow             ((>>>))
import Data.Scientific           (Scientific)
import qualified Data.Vector as V
import Lib (parseJSON', runCmd, Window, currentWorkspace, getAllWorkspaces, getWorkspaceByID, getWindows, enumerate)


data Direction = Left | Right
  deriving (Eq, Read)

instance Show Direction where
  show Left  = "left"
  show Right = "right"

flipDirection :: Direction -> Direction
flipDirection Left  = Right
flipDirection Right = Left

swapCmd :: Direction -> String
swapCmd direction =
  "mark --add _last; focus " <> show direction <> "; swap with mark \"_last\"; focus " <> show direction <> ";"

flipRepeatSwapCmd :: Direction -> Int -> String
flipRepeatSwapCmd direction times =
  join $ replicate times $ swapCmd $ flipDirection direction

i3Msg :: [String] -> IO (ExitCode, String, String)
i3Msg args = readProcessWithExitCode "i3-msg" args []

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      -- 1 is the leftmost key, not 0
      let dir = (read x :: Direction)
      (code_w, stdout_w, stderr_w) <- i3Msg ["-t", "get_workspaces"]
      (code_t, stdout_t, stderr_t) <- i3Msg ["-t", "get_tree"]
      case (code_w, code_t) of
        (ExitSuccess, ExitSuccess) -> currentWorkspace tryParse dir stdout_w stdout_t
        _                          -> putStrLn stderr_w >> putStrLn stderr_t
    _ -> pure ()

tryParse :: Direction -> Scientific -> String -> IO ()
tryParse dir workspace =
  (parseJSON'
  >=> getAllWorkspaces
  >=> getWorkspaceByID workspace
  >=> getWindows
  >>> fmap enumerate
  >>> fmap getFirstFocused'
  >=> generateCmd' dir)
  >>> runCmd

getFirstFocused' :: Vector (Int, Window) -> (Vector Window, Maybe (Int, Window))
getFirstFocused' = filterFocusedWindow >>> (\(a, b) -> (a, b !? 0))

-- Assumes invalid JSON is not focused
filterFocusedWindow :: Vector (Int, Window) -> (Vector Window, Vector (Int, Window))
filterFocusedWindow x = (fmap snd x, V.filter (\(_, window) ->
    case window ^? key "focused" of
      Just (Bool y) -> y
      _             -> False
  ) x)

generateCmd' :: Direction -> (Vector Window, Maybe (Int, Window)) -> Maybe String
generateCmd' dir (windows, m) =
  m >>= (\x -> Just $ "i3-msg \"" <> generateCmd dir (windows, x) <> "\"")

generateCmd :: Direction -> (Vector Window, (Int, Window)) -> String
generateCmd dir (windows, (idx, _)) =
  case (idx, dir) of
    (i, Right) | i + 1 == nwins -> flipRepeatSwapCmd dir idx
    (0, Left)                   -> flipRepeatSwapCmd dir (nwins - 1)
    _                           -> swapCmd dir
  where
    nwins = V.length windows

