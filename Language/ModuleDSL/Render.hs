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

instance ToJavascript Name where
  toJavascript (Name n) = toJavascript $ rawJS n

instance ToMarkup Value where
  toMarkup (String s) = toMarkup s
  toMarkup (Integer i) = toMarkup i
  toMarkup (Double d) = toMarkup d
  toMarkup (Bool b) = toMarkup b
  toMarkup Null = toMarkup ("null" :: String)


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
  render (name, NumericQuestion t os) =
    let Integer mn = lookupOpt "min" os (Integer 0)
        Integer mx = lookupOpt "max" os (Integer 100)
        initVal = (mn + mx) `div` 2
    in ([shamlet|
         <div .question>
           <div .question-text>
             #{t}
           <div>
             <input type="range" min=#{mn} max=#{mx}
                         ng-model="results['#{name}']">
             <span>
               {{results['#{name}']}}
        |],
        [julius|
console.log("Numeric question: #{name}");
sc.results['#{name}'] = #{toJSON initVal};
|])
  render (name, (ChoiceQuestion qt _os cs)) =
    ([shamlet|
      <div .question>
        <div .question-text>
          <span>
            #{qt}
          $forall Choice ct v <- cs
            <label .radio>
              <span>
                #{ct}
              <input type="radio" ng-model="results['#{name}']"
                     value=#{toMarkup v} name=#{"radio_" <> name}>
     |],
     [julius|
console.log("Choice question: #{name}");
sc.results['#{name}'] = null;
|])
