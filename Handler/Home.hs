{-# LANGUAGE TupleSections, ScopedTypeVariables, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  ms <- runDB $ selectList [] [Asc ModuleId]
  os <- runDB $ mapM get $ map (moduleOwner . entityVal) ms
  let onames = map (maybe "Unknown" userEmail) os
      modules = zip ms onames
  defaultLayout $(widgetFile "homepage")

entityToIntId :: KeyBackend b e -> Int
entityToIntId ent = do
  case fromPersistValue . unKey $ ent of
    Right (uid::Int) -> uid
    Left _           -> (-1)
