{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson                (decode, Value(Bool, Number, Array))
import Data.ByteString.Lazy.UTF8 (fromString)
import Control.Monad.Extra       (whenJust)
import System.Process            (callCommand)
import Control.Arrow             ((>>>))
import Control.Monad             ((>=>))
import Control.Lens              ((^?))
import Data.Vector               (Vector, (!?), enumFromN)
import Data.Aeson.Lens           (key, nth)
import Data.Scientific           (Scientific)
import qualified Data.Vector as V

parseJSON' :: String -> Maybe Value
parseJSON' = fromString >>> decode

type Workspace = Value
type Window = Value

runCmd :: Maybe String -> IO ()
runCmd = flip whenJust callCommand

getFirstFocusedWindow :: Vector Window -> Maybe Window
getFirstFocusedWindow = filterFocusedWindow >>> (!? 0)

-- Assumes invalid JSON is not focused
filterFocusedWindow :: Vector Window -> Vector Window
filterFocusedWindow =
  V.filter (\window ->
    case window ^? key "focused" of
      Just (Bool x) -> x
      _             -> False
  )

currentWorkspace :: (a -> Scientific -> String -> IO ()) -> a -> String -> String -> IO ()
currentWorkspace f a stdout_w stdout_t =
  case getCurrentWorkspaceID stdout_w of
    Just w  -> f a w stdout_t
    Nothing -> pure ()

getCurrentWorkspaceID :: String -> Maybe Scientific
getCurrentWorkspaceID =
  parseJSON' >=> parseArray >=> getFirstFocusedWindow >=> parseID

parseArray :: Value -> Maybe (Vector Workspace)
parseArray (Array x) = pure x
parseArray _         = Nothing

parseID :: Workspace -> Maybe Scientific
parseID json =
  case json ^? key "id" of
    Just (Number x) -> pure x
    _               -> Nothing

getAllWorkspaces :: Value -> Maybe (Vector Workspace)
getAllWorkspaces json =
  case json ^? key "nodes" . nth 1 . key "nodes" . nth 1 . key "nodes" of
    Just (Array x) -> pure x
    _              -> Nothing

getWorkspaceByID :: Scientific -> Vector Workspace -> Maybe Workspace
getWorkspaceByID idx = filterWorkspaceByID idx >>> (!? 0)

filterWorkspaceByID :: Scientific -> Vector Workspace -> Vector Workspace
filterWorkspaceByID idx = V.filter (\window ->
    case window ^? key "id" of
      Just (Number x) -> idx == x
      _             -> False
  )

getWindows :: Workspace -> Maybe (Vector Window)
getWindows json =
  case json ^? key "nodes" . nth 0 . key "nodes" of
    Just (Array x) -> pure x
    _              -> Nothing

enumerate :: Vector a -> Vector (Int, a)
enumerate windows = V.zip (enumFromN 0 (V.length windows)) windows
