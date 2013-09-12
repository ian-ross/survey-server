{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.ModuleDSL.Render where

import Import hiding (Module, Value, Null, String, Bool)
import Import (String)
import Data.List (intersperse)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Text.Blaze
import Text.Julius

import Language.ModuleDSL.Syntax

class Render a where
  render :: a -> Widget

instance ToMarkup Name where
  toMarkup (Name n) = toMarkup n

instance ToMarkup Value where
  toMarkup (String s) = toMarkup s
  toMarkup (Integer i) = toMarkup i
  toMarkup (Double d) = toMarkup d
  toMarkup (Bool b) = toMarkup b
  toMarkup Null = toMarkup ("null" :: String)

instance ToJavascript Value where
  toJavascript (String s) = singleton '"' <> fromText s <> singleton '"'
  toJavascript (Integer i) = decimal i
  toJavascript (Double d) = realFloat d
  toJavascript (Bool b) = if b then "true" else "false"
  toJavascript Null = "null"

instance Render Module where
  render (Module _name _opts body) = mapM_ render body

instance Render TopLevel where
  render (SurveyPage name _opts body) = do
    toWidget [hamlet|<h4>Survey page: #{toMarkup name}|]
    mapM_ render body
  render (Specialisation _name _params _body) =
    toWidget [hamlet|Specialisation...|]

instance Render (Name, Question) where
  render (n, NumericQuestion t os) = do
    let Integer mn = lookupOpt "min" os (Integer 0)
        Integer mx = lookupOpt "max" os (Integer 100)
        initVal = (mn + mx) `div` 2
        sname = "slider_" <> n
    toWidget [hamlet|
  <div .question>
    <div .question-text>
      #{t}
    <input type="range" min=#{mn} max=#{mx}
           ng-model="#{sname}" ng-init="#{sname}=#{initVal}">
    <span>
      {{#{sname}}}
             |]
  render (n, (ChoiceQuestion qt _os cs)) = do
    let rname = "radio_" <> n
    toWidget [hamlet|
  <div .question>
    <div .question-text>
      #{qt}
    $forall Choice ct _v <- cs
      <label .radio>
        <input type="radio" name="#{rname}">
          #{ct}
             |]
