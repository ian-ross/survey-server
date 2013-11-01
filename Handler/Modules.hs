{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Handler.Modules
       ( getModulesR
       , getModuleNewR, postModuleNewR
       , getModuleEditR, postModuleEditR
       , postModuleDeleteR
       , getModuleViewR
       , postModuleScheduleR
       ) where

import Import
import Layouts
import Utils
import Control.Monad (forM)
import Data.Time
import qualified Data.Text as T
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Julius
import System.Locale
import Angular.UIRouter
import qualified Language.ModuleDSL as ModDSL

getModulesR :: Handler Html
getModulesR = do
  ms <- runDB $ selectList [] [Asc ModuleId]
  os <- runDB $ mapM get $ map (moduleOwner . entityVal) ms
  let onames = map (maybe "Unknown" userEmail) os
      modules = zip ms onames
  appLayout $(widgetFile "module/list")

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
      redirectUltDest $ ModuleViewR moduleId
    _ -> do
      setAlert Error "Form error"
      redirectUltDest ModuleNewR

getModuleNewR :: Handler Html
getModuleNewR = do
  (formWidget, enctype) <- generateFormPost $ moduleForm Nothing
  appLayout $(widgetFile "module/new")

postModuleEditR :: ModuleId -> Handler Html
postModuleEditR moduleId = do
  uid <- requireAuthId
  ((formResult, _), _) <- runFormPost $ moduleForm Nothing
  case formResult of
    FormSuccess (t, c) -> do
      _ <- runDB $ replace moduleId $ Module t (unTextarea c) uid
      setAlert OK "Successfully replaced"
    _ -> setAlert Error "Form error"
  redirectUltDest $ ModuleViewR moduleId

getModuleEditR :: ModuleId -> Handler Html
getModuleEditR moduleId = do
  mdl <- runDB $ get404 moduleId
  (formWidget, enctype) <- generateFormPost $ moduleForm (Just mdl)
  appLayout $(widgetFile "module/edit")

postModuleDeleteR :: ModuleId -> Handler Html
postModuleDeleteR moduleId = do
  mdl <- runDB $ get404 moduleId
  runDB $ deleteWhere [ModuleActivationModule ==. moduleId]
  runDB $ deleteWhere [ModuleDataModule ==. moduleId]
  runDB $ delete moduleId
  setAlert OK $ "Deleted Module: " <> moduleTitle mdl
  redirectUltDest HomeR

data SurveyQ = SurveyQ { sqQ :: Text, sqA :: Text }

instance ToJSON SurveyQ where
  toJSON (SurveyQ q a) = object [ "question" .= q, "answer" .= a ]

data SurveyAct = SurveyAct { saUser :: Text,
                             saDate :: Text,
                             saQs :: [SurveyQ] }

instance ToJSON SurveyAct where
  toJSON (SurveyAct u d qs) =
    object [ "user" .= u, "date" .= d, "questions" .= qs ]

getModuleViewR :: ModuleId -> Handler Html
getModuleViewR moduleId = do
  mdl <- runDB $ get404 moduleId
  let parseResult = ModDSL.parseModule $ moduleContent mdl
      parseView = either (unlines . ModDSL.formatErrors) show parseResult
      ppResult = either (const "") (ModDSL.prettyPrint) parseResult
      (rawrendered, rawscripts) =
        case parseResult of
          Right m ->
            let (markup, rs) = ModDSL.render m
                json = toJSON $ renderHtml markup
            in ([julius|sc.render = $compile(#{json})(sc);|], rs)
          Left _ ->
            ([julius| sc.render = "Parse failed: can't render!";|], mempty)
  acts <- runDB $ selectList [ModuleActivationModule ==. moduleId,
                              ModuleActivationCompleted ==. True]
                             [Asc ModuleActivationDate]
  results <- forM acts $ \(Entity _ (ModuleActivation _ u d hash _)) -> do
    user <- runDB $ get u
    let username = maybe "Unknown" userEmail user
        ts = T.pack $ formatTime defaultTimeLocale "%c" d
    dat <- runDB $ selectList [ModuleDataHash ==. hash] []
    let qas = map (\(Entity _ (ModuleData _ _ q a)) -> SurveyQ q a) dat
    return $ SurveyAct username ts qas
  scripts <- renderJavascript <$> giveUrlRenderer rawscripts
  rendered <- renderJavascript <$> giveUrlRenderer rawrendered
  runAngularUIWithLayout appLayout $ do
    $(addSharedModule "postprocessor" True)
    $(buildStateUI "module-view")

postModuleScheduleR :: ModuleId -> Handler Html
postModuleScheduleR modid = do
  uid <- requireAuthId
  now <- liftIO $ getCurrentTime
  hash <- liftIO $ generateHash
  runDB $ insert_ $ ModuleActivation modid uid now hash False
  setAlert OK "Successfully scheduled"
  redirect $ HomeR
