{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.ModuleDSL.Render
       ( render
       ) where

import Import hiding (Module, Value, Null, String, Bool)
import Import (String)
import Text.Blaze
import Text.Julius

import Language.ModuleDSL.Syntax

class Render a where
  render :: a -> (Markup, JavascriptUrl url)

instance ToMarkup Name where
  toMarkup (Name n) = toMarkup n

instance ToValue Name where
  toValue (Name n) = toValue n

instance ToJavascript Name where
  toJavascript (Name n) = toJavascript $ rawJS n

instance ToValue Value where
  toValue (String s) = toValue s
  toValue (Integer i) = toValue i
  toValue (Double d) = toValue d
  toValue (Bool b) = toValue b
  toValue Null = toValue ("null" :: String)


instance Render Module where
  render (Module _name _opts body) = mconcat $ map render body

instance Render TopLevel where
  render (SurveyPage name _opts body) =
    let (qs, ss) = mconcat $ map render body
    in ([shamlet|
         <h4>
           Survey page: #{name}
         ^{qs}
        |],
        [julius|console.log("Survey page: #{name}");|] <> ss)
  render (Specialisation _name _params _body) =
    (toMarkup ("Specialisation..." :: String), mempty)

instance Render (Name, Question) where
  render (n, NumericQuestion t os) =
    let Integer mn = lookupOpt "min" os (Integer 0)
        Integer mx = lookupOpt "max" os (Integer 100)
        initVal = (mn + mx) `div` 2
        sname = "slider_" <> n
    in ([shamlet|
         <div .question>
           <div .question-text>
             #{t}
           <div>
             <input type="range" min=#{mn} max=#{mx}
                    ng-model="#{sname}" ng-init="#{sname}=#{initVal}">
             <span>
               {{#{sname}}}
        |],
        [julius|console.log("Numeric question: #{n}");|])
  render (n, (ChoiceQuestion qt _os cs)) =
    ([shamlet|
      <div .question>
        <div .question-text>
          <span>
            #{qt}
          $forall Choice ct _v <- cs
            <label .radio>
              <span>
                #{ct}
              <input type="radio" name=#{"radio_" <> n}>
     |],
     [julius|console.log("Choice question: #{n}");|])
