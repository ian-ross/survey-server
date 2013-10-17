{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.ModuleDSL.Render
       ( render
       ) where

import Import hiding (Module, Null, String, Bool)
import Import (String)
import Data.List (intersperse)
import qualified Data.Aeson as A
import Data.Attoparsec.Number (Number(..))
import Text.Blaze
import Text.Julius

import Language.ModuleDSL.Syntax

class Render a where
  render :: a -> (Markup, JavascriptUrl url)

instance ToMarkup Name where
  toMarkup (Name n) = toMarkup n

instance ToJavascript Name where
  toJavascript (Name n) = toJavascript $ rawJS n

instance ToJavascript [Name] where
  toJavascript ns = mconcat $ intersperse "," $ map (toJavascript . rawJS) ns

instance ToMarkup Literal where
  toMarkup (String s) = toMarkup s
  toMarkup (Integer i) = toMarkup i
  toMarkup (Double d) = toMarkup d
  toMarkup (Bool b) = toMarkup b
  toMarkup Null = toMarkup ("null" :: String)

instance ToJavascript Literal where
  toJavascript (String s) = toJavascript $ A.String s
  toJavascript (Integer i) = toJavascript $ A.Number (I i)
  toJavascript (Double d) = toJavascript $ A.Number (D d)
  toJavascript (Bool b) = toJavascript $ A.Bool b
  toJavascript Null = toJavascript $ A.Null

instance ToJavascript UnaryOp where
  toJavascript NegOp = "-"
  toJavascript NotOp = "!"

instance ToJavascript BinaryOp where
  toJavascript AddOp = "+"
  toJavascript SubOp = "-"
  toJavascript MulOp = "*"
  toJavascript DivOp = "/"
  toJavascript PowOp = "^"       -- WRONG!
  toJavascript CatOp = "+"
  toJavascript AndOp = "&&"
  toJavascript OrOp = "||"
  toJavascript EqOp = "=="
  toJavascript NEqOp = "!="
  toJavascript GtOp = ">"
  toJavascript GEqOp = ">="
  toJavascript LtOp = "<"
  toJavascript LeqOp = "<="
  toJavascript EqCIOp = "@=="    -- WRONG!
  toJavascript NEqCIOp = "@!="   -- WRONG!
  toJavascript GtCIOp = "@>"     -- WRONG!
  toJavascript GEqCIOp = "@>="   -- WRONG!
  toJavascript LtCIOp = "@<"     -- WRONG!
  toJavascript LeqCIOp = "@<="   -- WRONG!

class Fixity a where
  fixity :: a -> Maybe Int

instance Fixity Expr where
  fixity (BinaryExpr op _ _) = fixity op
  fixity _ = Nothing

instance Fixity BinaryOp where
  fixity PowOp = Just 9
  fixity MulOp = Just 8
  fixity DivOp = Just 8
  fixity AddOp = Just 7
  fixity SubOp = Just 7
  fixity AndOp = Just 5
  fixity OrOp  = Just 5
  fixity _     = Just 6         -- Comparison operators.

data Assoc = L | R | None deriving Eq
assoc :: BinaryOp -> Assoc
assoc PowOp = R
assoc MulOp = L
assoc DivOp = L
assoc AddOp = L
assoc SubOp = L
assoc AndOp = L
assoc OrOp  = L
assoc _     = None              -- Comparison operators.

instance ToJavascript Choice where
  toJavascript (Choice t v) =
    "{ label: " <> toJavascript (A.String t) <>
    ", value: " <> toJavascript v <> "}"

instance ToJavascript [Choice] where
  toJavascript cs =
    "[" <> (mconcat $ intersperse "," $ map toJavascript cs) <> "]"

instance ToJavascript Expr where
  toJavascript (LitExpr lit) = toJavascript lit
  toJavascript (RefExpr n) = "sc." <> toJavascript n
  toJavascript (UnaryExpr op e@(LitExpr _)) = toJavascript op <> toJavascript e
  toJavascript (UnaryExpr op e@(RefExpr _)) = toJavascript op <> toJavascript e
  toJavascript (UnaryExpr op e) =
    toJavascript op <> "(" <> toJavascript e <> ")"
  toJavascript (BinaryExpr PowOp e1 e2) =
    "Math.pow(" <> toJavascript e1 <> "," <> toJavascript e2 <> ")"
  toJavascript (BinaryExpr op e1 e2) =
    let Just opfix = fixity op
        js1 = case fixity e1 of
          Nothing -> toJavascript e1
          Just f1 -> if f1 < opfix || (f1 == opfix && assoc op == L)
                     then "(" <> toJavascript e1 <> ")"
                     else toJavascript e1
        js2 = case fixity e2 of
          Nothing -> toJavascript e2
          Just f2 -> if f2 < opfix || (f2 == opfix && assoc op == R)
                     then "(" <> toJavascript e2 <> ")"
                     else toJavascript e2
    in js1 <> toJavascript op <> js2
  toJavascript (FunExpr n es) =
    "sc." <> toJavascript n <> "(" <>
    mconcat (intersperse "," $ map toJavascript es) <> ")"
  toJavascript (IfThenElseExpr i t e) =
    "((" <> toJavascript i <> ") ? (" <>
         toJavascript t <> ") : (" <> toJavascript e <> "))"

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
  render (Function name params body) =
    (mempty, [julius|
  sc.#{name} = function(#{params}) {
    return #{body};
  };
             |])

-- RENDER NEEDS TO HAVE SOME SORT OF MONADIC NAME SUPPLY...

instance Render Question where
  render (NumericQuestion name t os) =
    let LitExpr (Integer mn) = lookupOpt "min" os (LitExpr $ Integer 0)
        LitExpr (Integer mx) = lookupOpt "max" os (LitExpr $ Integer 100)
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
  render (ChoiceQuestion name qt _os cs) =
    ([shamlet|
      <div .question>
        <div .question-text>
          <span>
            #{qt}
          $forall Choice ct (LitExpr v) <- cs
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
  render (DropdownQuestion name qt _os cs) =
    ([shamlet|
      <div .question>
        <div .question-text>
          <span>
            #{qt}
          <select ng-model="results['#{name}']"
                  ng-options="c.label for c in choices">
     |],
     [julius|
console.log("Dropdown question: #{name}");
sc.choices = #{cs};
sc.results['#{name}'] = sc.choices[0];
|])
  render (TextDisplay qt _os) =
    ([shamlet|
      <div .question>
        <div .question-text>
          <span>
            {{text[0]}}
     |],
     [julius|
console.log("Text display...");
sc.text[0] = #{qt};
|])
