{-# LANGUAGE TupleSections, ScopedTypeVariables, OverloadedStrings #-}
-- | Handler for main page.
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

-- | Front page: displays a list of the user's outstanding module
-- activations (i.e. surveys to be run) and completed surveys.
--
-- Hamlet and Julius in: angular/ui-router/home
getHomeR :: Handler Html
getHomeR = do
  uid <- requireAuthId

  -- Get all modules and module names from database.
  ms <- runDB $ selectList [] [Asc ModuleId]
  let mnames = M.fromList $ map (\(Entity mid m) -> (mid, moduleTitle m)) ms

  -- Get users module activations.
  as <- runDB $ map entityVal <$>
        selectList [ModuleActivationUser ==. uid] [Asc ModuleActivationDate]

  -- Partition module activations into completed and outstanding and
  -- reorganised module details into form suitable for rendering page.
  let (compas, outas) = partition moduleActivationCompleted as
      outstanding = map (extract mnames) outas
      completed = map (extract mnames) compas

  -- Render page.
  runAngularUIWithLayout appLayout $ do
    $(addSharedModule "alerts" True)
    $(buildStateUI "home")

-- | Extract module name to go with module activation and format data
-- for display in client.
extract :: Map ModuleId Text -> ModuleActivation -> (Text, Text, Text, Text)
extract mnames (ModuleActivation mid _ date hash _) =
  ( T.pack $ show $ entityToIntId mid
  , mnames M.! mid
  , T.pack $ formatTime defaultTimeLocale "%c" date
  , hash )
