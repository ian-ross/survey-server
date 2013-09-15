{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
module Handler.Survey
  ( getSurveyRunR
  , getSurveyResultsR
  )
  where

import Import
import Layouts


getSurveyRunR :: Text -> Handler Html
getSurveyRunR hash = do
  appLayout [whamlet|Run #{hash}|]

getSurveyResultsR :: Text -> Handler Html
getSurveyResultsR hash = do
  appLayout [whamlet|Results for #{hash}|]
