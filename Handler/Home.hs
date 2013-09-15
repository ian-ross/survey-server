{-# LANGUAGE TupleSections, ScopedTypeVariables, OverloadedStrings #-}
module Handler.Home
       ( getHomeR
       ) where

import Import
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (partition)
import Data.Time
import System.Locale

import Angular.UIRouter
import Layouts
import Utils

getHomeR :: Handler Html
getHomeR = do
  uid <- requireAuthId
  ms <- runDB $ selectList [] [Asc ModuleId]
  let mnames = M.fromList $ map (\(Entity mid m) -> (mid, moduleTitle m)) ms
  as <- runDB $ map entityVal <$>
        selectList [ModuleActivationUser ==. uid] [Asc ModuleActivationDate]
  let (compas, outas) = partition moduleActivationCompleted as
      outstanding = map (extract mnames) outas
      completed = map (extract mnames) compas
  runAngularUIWithLayout appLayout $ do
    $(addSharedModule "alerts" True)
    $(buildStateUI "home")

extract :: Map ModuleId Text -> ModuleActivation -> (Text, Text, Text, Text)
extract mnames (ModuleActivation mid _ date hash _) =
  ( T.pack $ show $ entityToIntId mid
  , mnames M.! mid
  , T.pack $ formatTime defaultTimeLocale "%c" date
  , hash )
