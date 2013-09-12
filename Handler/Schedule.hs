{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
module Handler.Schedule
  ( postScheduleR
--  , getScheduleListR
  )
  where

import Import
import Data.Time
import Utils


postScheduleR :: ModuleId -> Handler Html
postScheduleR modid = do
  uid <- requireAuthId
  now <- liftIO $ getCurrentTime
  hash <- liftIO $ generateHash
  runDB $ insert_ $ ModuleActivation modid uid now hash
  setAlert OK "Successfully scheduled"
  redirectUltDest $ HomeR
