{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
module Handler.CRUD
  ( postModuleNewR
  , getModuleNewR
  , postModuleEditR
  , getModuleEditR
  , postModuleDeleteR
  , getModuleDetailR
  )
  where

import Import
import Utils
import qualified Data.Text as T
import qualified Language.ModuleDSL as ModDSL


moduleForm :: Maybe Module -> Form (Text, Textarea)
moduleForm mModule =
  renderBootstrap $ (,)
    <$> areq textField "Title" (moduleTitle <$> mModule)
    <*> areq textareaField "Content" ((Textarea . moduleContent) <$> mModule)

postModuleNewR :: Handler Html
postModuleNewR = do
  uid <- requireAuthId
  ((formResult, _), _) <- runFormPost $ moduleForm Nothing
  case formResult of
    FormSuccess (t, c) -> do
      moduleId <- runDB $ insert $ Module t (unTextarea c) uid
      setAlert OK "Successfully added"
      redirectUltDest $ ModuleDetailR moduleId
    _ -> do
      setAlert Error "Form error"
      redirectUltDest ModuleNewR

getModuleNewR :: Handler Html
getModuleNewR = do
  (formWidget, enctype) <- generateFormPost $ moduleForm Nothing
  defaultLayout $(widgetFile "module/new")

postModuleEditR :: ModuleId -> Handler Html
postModuleEditR moduleId = do
  uid <- requireAuthId
  ((formResult, _), _) <- runFormPost $ moduleForm Nothing
  case formResult of
    FormSuccess (t, c) -> do
      _ <- runDB $ replace moduleId $ Module t (unTextarea c) uid
      setAlert OK "Successfully replaced"
    _ -> setAlert Error "Form error"
  redirectUltDest $ ModuleDetailR moduleId

getModuleEditR :: ModuleId -> Handler Html
getModuleEditR moduleId = do
  mdl <- runDB $ get404 moduleId
  (formWidget, enctype) <- generateFormPost $ moduleForm (Just mdl)
  defaultLayout $(widgetFile "module/edit")

postModuleDeleteR :: ModuleId -> Handler Html
postModuleDeleteR moduleId = do
  mdl <- runDB $ get404 moduleId
  runDB $ delete moduleId
  setAlert OK $ "Deleted Module: " <> moduleTitle mdl
  redirectUltDest HomeR

getModuleDetailR :: ModuleId -> Handler Html
getModuleDetailR moduleId = do
  mdl <- runDB $ get404 moduleId
  let parseResult = ModDSL.parseModule $ T.unpack $ moduleContent mdl
  let parseView = either (unlines . ModDSL.formatErrors) show parseResult
  let ppResult = either (const "") (ModDSL.prettyPrint) parseResult
  defaultLayout $(widgetFile "module/show")
