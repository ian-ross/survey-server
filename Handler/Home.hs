{-# LANGUAGE TupleSections, ScopedTypeVariables, OverloadedStrings #-}
module Handler.Home where

import Import
import Layouts


getHomeR :: Handler Html
getHomeR = do
  ms <- runDB $ selectList [] [Asc ModuleId]
  os <- runDB $ mapM get $ map (moduleOwner . entityVal) ms
  let onames = map (maybe "Unknown" userEmail) os
      modules = zip ms onames
  appLayout $(widgetFile "homepage")
