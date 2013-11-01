{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
-- | Handlers for viewing and manipulating module definitions and
-- results.
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

-- | Handler for main module list page: get all modules from database
-- along with owners, format for display and use simple Hamlet
-- template (templates/module/list.hamlet).
getModulesR :: Handler Html
getModulesR = do
  ms <- runDB $ selectList [] [Asc ModuleId]
  os <- runDB $ mapM get $ map (moduleOwner . entityVal) ms
  let onames = map (maybe "Unknown" userEmail) os
      modules = zip ms onames
  appLayout $(widgetFile "module/list")

-- | Form for editing module definitions: just the module name plus a
-- textarea for the definition.
moduleForm :: Maybe Module -> Form (Text, Textarea)
moduleForm mModule =
  renderBootstrap $ (,)
    <$> areq textField "Title" (moduleTitle <$> mModule)
    <*> areq textareaField "Content" ((Textarea . moduleContent) <$> mModule)

-- | GET handler for "New module": just render module form and serve
-- in a simple template (templates/module/new.hamlet).
getModuleNewR :: Handler Html
getModuleNewR = do
  (formWidget, enctype) <- generateFormPost $ moduleForm Nothing
  appLayout $(widgetFile "module/new")

-- | POST handler for "New module": run the form, extract the results
-- and try to add the new module to the database.  Use alerts to
-- inform the user what happened and redirect either to the module
-- view page for the new module or back to the "New module" page.
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

-- | GET handler for "Edit module": get the module from the database,
-- render the module form and serve in a simple template
-- (templates/module/edit.hamlet).
getModuleEditR :: ModuleId -> Handler Html
getModuleEditR moduleId = do
  mdl <- runDB $ get404 moduleId
  (formWidget, enctype) <- generateFormPost $ moduleForm (Just mdl)
  appLayout $(widgetFile "module/edit")

-- | POST handler for "Edit module": more or less the same as for "New
-- module".
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

-- | POST handler for "Delete module": find the module, clean up any
-- dependencies (module activations or data) in the database, then
-- delete the module.
--
-- NOTE: THIS IS CURRENTLY COMPLETELY INSECURE.  ANYONE CAN DELETE
-- ANYONE ELSE'S MODULES!  FOR DEVELOPMENT USE ONLY!
postModuleDeleteR :: ModuleId -> Handler Html
postModuleDeleteR moduleId = do
  mdl <- runDB $ get404 moduleId
  runDB $ deleteWhere [ModuleActivationModule ==. moduleId]
  runDB $ deleteWhere [ModuleDataModule ==. moduleId]
  runDB $ delete moduleId
  setAlert OK $ "Deleted Module: " <> moduleTitle mdl
  redirectUltDest HomeR

-- | POST handler for "Schedule" links on module list: just generate a
-- random hash and add an entry to the module activation table for the
-- appropriate module.
postModuleScheduleR :: ModuleId -> Handler Html
postModuleScheduleR modid = do
  uid <- requireAuthId
  now <- liftIO $ getCurrentTime
  hash <- liftIO $ generateHash
  runDB $ insert_ $ ModuleActivation modid uid now hash False
  setAlert OK "Successfully scheduled"
  redirect $ HomeR


-- | Survey result data type used for module view pages.  Basically
-- just a carrier for a ToJSON instance.
data SurveyQ = SurveyQ { sqQ :: Text, sqA :: Text }

instance ToJSON SurveyQ where
  toJSON (SurveyQ q a) = object [ "question" .= q, "answer" .= a ]

-- | Survey activation data type used for module view pages.
-- Basically just a carrier for a ToJSON instance.
data SurveyAct = SurveyAct { saUser :: Text,
                             saDate :: Text,
                             saQs :: [SurveyQ] }

instance ToJSON SurveyAct where
  toJSON (SurveyAct u d qs) =
    object [ "user" .= u, "date" .= d, "questions" .= qs ]

-- | GET handler for module view pages: this uses an Angular ui-router
-- state-driven system to present a number of different views of
-- module information.
getModuleViewR :: ModuleId -> Handler Html
getModuleViewR moduleId = do
  -- Get the module from the database.
  mdl <- runDB $ get404 moduleId

  -- Parse the module definition, producing a representation of the
  -- raw AST along with a pretty-printed version of the AST.
  let parseResult = ModDSL.parseModule $ moduleContent mdl
      parseView = either (unlines . ModDSL.formatErrors) show parseResult
      ppResult = either (const "") (ModDSL.prettyPrint) parseResult

  -- Render the module and add some Javascript to compile the page
  -- contents to a form where it can be used in the test-render panel.
  let (rawrendered, rawscripts) =
        case parseResult of
          Right m ->
            let (markup, rs) = ModDSL.render m
                json = toJSON $ renderHtml markup
            in ([julius|sc.render = $compile(#{json})(sc);|], rs)
          Left _ ->
            ([julius| sc.render = "Parse failed: can't render!";|], mempty)

  -- Get all the completed surveys for this module and extract their
  -- results from the database, reformatting them as SurveyAct values
  -- to pass to the client.
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

  -- Convert the module content into a form that can be incorporated
  -- into Hamlet and Julius templates.
  scripts <- renderJavascript <$> giveUrlRenderer rawscripts
  rendered <- renderJavascript <$> giveUrlRenderer rawrendered

  -- Run the Angular pages: the Hamlet and Julius files for this live
  -- under angular/ui-router/module-view
  runAngularUIWithLayout appLayout $ do
    $(addSharedModule "postprocessor" True)
    $(buildStateUI "module-view")
