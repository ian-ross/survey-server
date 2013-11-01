{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
-- | Handlers for running surveys and viewing survey results.
module Handler.Survey
  ( getSurveyRunR, postSurveyRunR
  , postSurveyDeleteR
  , getSurveyResultsR
  )
  where

import Import
import Angular.UIRouter
import Layouts
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Julius
import qualified Data.Aeson as A
import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time
import System.Locale
import qualified Language.ModuleDSL as ModDSL


-- | GET handler for main survey links: the input parameter is the
-- random hash for the survey activation.  Error checking here could
-- be better: "slopey code" anti-pattern in evidence.
getSurveyRunR :: Text -> Handler Html
getSurveyRunR hash = do
  -- Get the module activation information from the database.
  mmodact <- runDB $ selectFirst [ModuleActivationHash ==. hash] []

  case mmodact of
    -- No record of this survey activation hash.
    Nothing -> missingSurvey
    Just (Entity _ modact) -> do
      -- Get module details.
      mmdl <- runDB $ get (moduleActivationModule modact)
      case mmdl of
        -- Deal with modules that have been deleted from the database
        -- (shouldn't happen: the activation hashes should also be
        -- deleted if the module is deleted).
        Nothing -> missingModule
        Just mdl -> do
          -- Try to parse the module contents.
          let parseResult = ModDSL.parseModule $ moduleContent mdl
          case parseResult of
            -- Deal with parse failures: ideally this also should not
            -- happen, since the module should ave been parsed
            -- correctly before the module activation was generated.
            Left _ -> badModule
            Right par -> do
              -- Render the module, convert HTML and Javascript to
              -- values usable in Shakespeare templates and send the
              -- run-survey Angular ui-router pages to the client.
              -- (Hamlet and Julius files for this are in
              -- angular/ui-router/run-survey and are responsible for
              -- setting up a suitable Angular environment for the
              -- rendered module code to run, and then marshalling and
              -- returning results once the survey is completed.)
              let (markup, rawscripts) = ModDSL.render par
                  rendered = renderHtml markup
              scripts <- renderJavascript <$> giveUrlRenderer rawscripts
              runAngularUIWithLayout appLayout $ do
                $(addSharedModule "alerts" True)
                $(addSharedModule "postprocessor" True)
                $(buildStateUI "run-survey")

-- | Handy type synonym.
type Ret = Either Text Text

-- | POST handler for dealing with survey results: the Text parameter
-- here is the module activation hash and the results themselves are
-- passed as the JSON body of the POST request.
postSurveyRunR :: Text -> Handler Value
postSurveyRunR hash = do
  -- Get the module activation record from the database.
  mmodact <- runDB $ selectFirst [ModuleActivationHash ==. hash] []

  case mmodact of
    -- No record of this survey activation hash: since this handler is
    -- called via Ajax, we return our error message as a JSON value.
    Nothing -> returnJson (Left missingSurveyMsg :: Ret)
    Just (Entity modactid modact) -> do
      let mdlid = moduleActivationModule modact

      -- Parse the JSON payload.
      rres <- parseJsonBody
      case rres of
        A.Error err -> returnJson (Left (T.pack err) :: Ret)
        A.Success (res :: Map Text Value) -> do
          -- Convert the fields of the JSON result to ModuleData,
          -- insert them into the database and mark that this module
          -- activation is complete.
          runDB $ do
            void $ insertMany $ map (conv mdlid) (M.toList res)
            update modactid [ModuleActivationCompleted =. True]
          returnJson (Right "OK" :: Ret)
  where conv m (k, v) = ModuleData hash m k (valToText v)
        valToText = LT.toStrict . decodeUtf8 . A.encode

-- | POST handler to deal with deletion of survey results.
postSurveyDeleteR :: Text -> Handler Html
postSurveyDeleteR hash = do
  uid <- requireAuthId
  mmodact <- runDB $ selectFirst [ModuleActivationHash ==. hash] []
  case mmodact of
    Nothing -> missingSurvey
    Just (Entity modactid modact) ->
      if moduleActivationUser modact /= uid
      then notYours
      else do
        runDB $ delete modactid
        runDB $ deleteWhere [ModuleDataHash ==. hash]
        redirect HomeR

-- | GET handler to access results for a given module activation hash.
getSurveyResultsR :: Text -> Handler Html
getSurveyResultsR hash = do
  mmodact <- runDB $ selectFirst [ModuleActivationHash ==. hash] []
  case mmodact of
    Nothing -> missingSurvey
    Just (Entity modactid modact) -> do
      mmdl <- runDB $ get (moduleActivationModule modact)
      case mmdl of
        Nothing -> missingModule
        Just mdl -> do
          ress <- runDB $ selectList [ModuleDataHash ==. hash] []
          let modName = moduleTitle mdl
              date = moduleActivationDate modact
              ts = T.pack $ formatTime defaultTimeLocale "%c" date
              res = map formatResult ress
          appLayout $(widgetFile "survey-results")
    where formatResult (Entity _ (ModuleData _ _ q v)) = (q, v)

-- | Error message used a couple of different ways.
missingSurveyMsg :: Text
missingSurveyMsg = "The survey link you followed no longer exists."

-- | Simple wrapper for error message handlers.
surveyError :: Text -> Handler Html
surveyError msg = appLayout [whamlet|
  <h4>Invalid survey link!

  <p>#{msg}
|]

-- | Handlers to generate error messages.
missingSurvey, missingModule, badModule, badResults, notYours :: Handler Html
missingSurvey = surveyError missingSurveyMsg
missingModule =
  surveyError $ "The module for the survey link you " <>
                "followed no longer exists."
badModule =
  surveyError $ "There was a problem parsing the module " <>
                "for the survey link you followed."
badResults =
  surveyError $ "Something went wrong with the transfer of " <>
                "results from the client!"
notYours = surveyError "That's not your data to delete!"
