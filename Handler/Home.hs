{-# LANGUAGE TupleSections, ScopedTypeVariables, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  ms <- runDB $ selectList [] [Asc ModuleId]
  let is = map (show . entityToIntId . entityKey) ms
      ts = map (moduleTitle . entityVal) ms
      oids = map (moduleOwner . entityVal) ms
  os <- runDB $ mapM get oids
  let onames = map (maybe "Unknown" userEmail) os
      modules = zip3 is ts onames
  defaultLayout $ do
    setTitle "Prototype Survey Server"
    $(widgetFile "homepage")

entityToIntId :: KeyBackend b e -> Int
entityToIntId ent = do
  case fromPersistValue . unKey $ ent of
    Right (uid::Int) ->  uid
