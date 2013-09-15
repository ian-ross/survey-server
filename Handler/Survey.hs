{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
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


getSurveyRunR :: Text -> Handler Html
getSurveyRunR hash = do
  mmodact <- runDB $ selectFirst [ModuleActivationHash ==. hash] []
  case mmodact of
    Nothing -> missingSurvey
    Just (Entity _ modact) -> do
      mmdl <- runDB $ get (moduleActivationModule modact)
      case mmdl of
        Nothing -> missingModule
        Just mdl -> do
          let parseResult = ModDSL.parseModule $ moduleContent mdl
          case parseResult of
            Left _ -> badModule
            Right par -> do
              let (markup, rawscripts) = ModDSL.render par
                  rendered = renderHtml markup
              scripts <- renderJavascript <$> giveUrlRenderer rawscripts
              runAngularUIWithLayout appLayout $ do
                $(addSharedModule "alerts" True)
                $(buildStateUI "run-survey")

type Ret = Either Text Text

postSurveyRunR :: Text -> Handler Value
postSurveyRunR hash = do
  mmodact <- runDB $ selectFirst [ModuleActivationHash ==. hash] []
  case mmodact of
    Nothing -> returnJson (Left missingSurveyMsg :: Ret)
    Just (Entity modactid modact) -> do
      let mdlid = moduleActivationModule modact
      rres <- parseJsonBody
      case rres of
        A.Error err -> returnJson (Left (T.pack err) :: Ret)
        A.Success (res :: Map Text Value) -> do
          runDB $ do
            void $ insertMany $ map (conv mdlid) (M.toList res)
            update modactid [ModuleActivationCompleted =. True]
          returnJson (Right "OK" :: Ret)
  where conv m (k, v) = ModuleData hash m k (valToText v)
        valToText = LT.toStrict . decodeUtf8 . A.encode


postSurveyDeleteR :: Text -> Handler Html
postSurveyDeleteR hash = do
  uid <- requireAuthId
  mmodact <- runDB $ selectFirst [ModuleActivationHash ==. hash] []
  case mmodact of
    Nothing -> missingSurvey
    Just (Entity modactid modact) ->
      if moduleActivationUser modact /= uid
      then notYourSurvey
      else do
        runDB $ delete modactid
        runDB $ deleteWhere [ModuleDataHash ==. hash]
        redirect HomeR


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


surveyError :: Text -> Handler Html
surveyError msg = appLayout [whamlet|
  <h4>Invalid survey link!

  <p>#{msg}
|]


missingSurveyMsg, missingModuleMsg :: Text
missingSurvey, missingModule, badModule, badResults :: Handler Html
missingSurveyMsg = "The survey link you followed no longer exists."
missingModuleMsg =
  "The module for the survey link you followed no longer exists."
missingSurvey = surveyError missingSurveyMsg
missingModule = surveyError missingModuleMsg
badModule =
  surveyError $ "There was a problem parsing the module " <>
                "for the survey link you followed."
badResults =
  surveyError $ "Something went wrong with the transfer of " <>
                "results from the client!"

notYourSurvey :: Handler Html
notYourSurvey = surveyError "That's not your data to delete!"
