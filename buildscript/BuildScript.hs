{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BuildScript where

import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time
import Optics
import Relude
import System.Process qualified as Process

data LogLevel = DEBUG | INFO | WARNING | ERROR deriving (Show, Eq)

log :: (MonadIO m) => LogLevel -> Text -> m ()
log level t = do
  now <- liftIO getZonedTime
  let message = show now <> " | " <> show level <> " | " <> t
  putTextLn message

logInfo :: (MonadIO m) => Text -> m ()
logInfo = log INFO

logError :: (MonadIO m) => Text -> m ()
logError = log ERROR

logLicense :: (MonadIO m) => m ()
logLicense = do
  license <- readFileBS "COPYING"
  logInfo (T.take 800 . decodeUtf8 $ license)

newtype Command = Command {value :: Text} deriving (Ord, Eq, Show)

makeFieldLabelsNoPrefix ''Command

newtype Description = Description {value :: Text} deriving (Ord, Eq, Show)

makeFieldLabelsNoPrefix ''Description

runTimed :: (MonadIO m) => m a -> m ()
runTimed eff = do
  start <- liftIO getCurrentTime
  _ <- eff
  end <- liftIO getCurrentTime
  let elapsed = diffUTCTime end start
  _ <- logInfo $ "🕙 completed in " <> show elapsed
  pure ()

runCommand :: (MonadIO m) => Description -> Command -> m ()
runCommand d c = do
  _ <- logInfo (d ^. #value)
  runTimed . liftIO . Process.callCommand . T.unpack $ c ^. #value

newtype Path = Path {value :: Text} deriving (Ord, Eq, Show)

makeFieldLabelsNoPrefix ''Path

removePaths :: (MonadIO m) => [Path] -> m ()
removePaths = mapM_ runRemoveCommand
  where
    runRemoveCommand x =
      runCommand
        (Description $ "🔨 removing path " <> x ^. #value)
        (Command $ "sudo rm -rf " <> x ^. #value)

copyPaths :: (MonadIO m) => Map Path Path -> m ()
copyPaths xs = mapM_ runCopyCommand $ Map.assocs xs
  where
    runCopyCommand (k, v) =
      runCommand
        (Description $ "🔨 copying path " <> k ^. #value <> " to " <> v ^. #value)
        (Command $ "sudo cp -rf " <> k ^. #value <> " " <> v ^. #value)

addToUpdateMoments :: (MonadIO m) => m ()
addToUpdateMoments = do
  now <- liftIO getCurrentTime
  appendFileText "./buildscript/update-dates" (T.pack . show $ now)
  pure ()

addToGarbageCollectedMoments :: (MonadIO m) => m ()
addToGarbageCollectedMoments = do
  now <- liftIO getCurrentTime
  appendFileText "./buildscript/garbage-collect-dates" (T.pack . show $ now)
  pure ()

getLatestUpdateMoment :: (MonadIO m) => m (Maybe UTCTime)
getLatestUpdateMoment = do
  fc <- readFileBS "./buildscript/update-dates"
  let fc' = decodeUtf8' fc
  case fc' of
    Left e -> (liftIO . putTextLn . T.pack . show $ e) >> pure Nothing
    Right c -> pure . readMaybe . T.unpack $ c

getLatestGarbageCollectedMoment :: (MonadIO m) => m (Maybe UTCTime)
getLatestGarbageCollectedMoment = do
  fc <- readFileBS "./buildscript/garbage-collect-dates"
  let fc' = decodeUtf8' fc
  case fc' of
    Left e -> (liftIO . putTextLn . T.pack . show $ e) >> pure Nothing
    Right c -> pure . readMaybe . T.unpack $ c

notSameDay :: (MonadIO m) => Maybe UTCTime -> m Bool
notSameDay Nothing = pure True
notSameDay (Just utcTime) = do
  now <- liftIO getCurrentTime
  pure $ day utcTime /= day now
  where
    day :: UTCTime -> Day
    day = day
