{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.ModuleDSL.Render
       ( render
       ) where

import Import hiding (Module, Null, String, Bool)
import Import (String)
import qualified Import as I
import Data.List (intersperse)
import qualified Data.Aeson as A
import Data.Attoparsec.Number (Number(..))
import Data.Text.Lazy.Builder
import Text.Blaze
import Text.Julius
import Control.Monad.Trans.State

import Language.ModuleDSL.Syntax

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
  toJavascript (RefExpr n) = toJavascript n
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
    toJavascript n <> "(" <>
    mconcat (intersperse "," $ map toJavascript es) <> ")"
  toJavascript (IfThenElseExpr i t e) =
    "((" <> toJavascript i <> ") ? (" <>
         toJavascript t <> ") : (" <> toJavascript e <> "))"
  toJavascript (RecordExpr fs) =
    "{" <> (mconcat $ intersperse "," $
            map (\(k, v) -> toJavascript k <> "=" <> toJavascript v) fs) <> "}"

data RenderState = RenderState { textIdx :: Integer
                               , utilIdx :: Integer
                               } deriving (Eq, Show)

initRenderState :: RenderState
initRenderState = RenderState 0 0

nextTextIdx :: State RenderState Integer
nextTextIdx = do
  tidx <- gets textIdx
  modify (\s -> s { textIdx = tidx + 1 })
  return tidx

nextUtilIdx :: State RenderState Integer
nextUtilIdx = do
  uidx <- gets utilIdx
  modify (\s -> s { utilIdx = uidx + 1 })
  return uidx


render :: RunRender a => a -> (Markup, JavascriptUrl url)
render v = evalState (runRender v) initRenderState

type Render url = State RenderState (Markup, JavascriptUrl url)

class RunRender a where
  runRender :: a -> Render url


instance RunRender Module where
  runRender (Module _name _opts body) = mconcat <$> mapM runRender body


instance RunRender TopLevel where runRender = renderTopLevel

renderTopLevel :: TopLevel -> Render url

renderTopLevel (SurveyPage name _opts body) = do
  (qs, ss) <- mconcat <$> mapM runRender body
  return ([shamlet|
  <h4>
    Survey page: #{name}
  ^{qs}
          |],
          [julius|console.log("Survey page: #{name}");|] <> ss)

renderTopLevel (Specialisation _name _params _body) =
  return (toMarkup ("Specialisation..." :: String), mempty)

renderTopLevel (Function name params body) =
  return (mempty,
          [julius|
  sc.#{name} = function(#{params}) {
    return #{fscope False params body};
  };
          |])

class Scopeable a where
  fscope :: I.Bool -> [Name] -> a -> a

instance Scopeable Name where
  fscope interp ls n@(Name nt)
    | n `notElem` ls =
        Name $ (if interp then "" else "sc.") <> "results['" <> nt <> "']"
    | otherwise      = n

fscope' :: I.Bool -> [Name] -> Name -> Name
fscope' interp ls n@(Name nt)
  | not interp && n `notElem` ls = Name $ "sc." <> nt
  | otherwise      = n

instance Scopeable Expr where
  fscope i ls (RefExpr n) = RefExpr (fscope i ls n)
  fscope i ls (UnaryExpr op e) = UnaryExpr op (fscope i ls e)
  fscope i ls (BinaryExpr op e1 e2) =
    BinaryExpr op (fscope i ls e1) (fscope i ls e2)
  fscope i ls (FunExpr n es) = FunExpr (fscope' i ls n) $ map (fscope i ls) es
  fscope i ls (IfThenElseExpr c t e) =
    IfThenElseExpr (fscope i ls c) (fscope i ls t) (fscope i ls e)
  fscope i ls (RecordExpr fs) =
    RecordExpr $ map (\(k, v) -> (k, fscope i ls v)) fs
  fscope _ _  (LitExpr l) = LitExpr l


instance RunRender Question where runRender = renderQuestion

renderQuestion :: Question -> Render url

renderQuestion (NumericQuestion name qt os) = do
  let LitExpr (Integer mn) = lookupOpt "min" os (LitExpr $ Integer 0)
      LitExpr (Integer mx) = lookupOpt "max" os (LitExpr $ Integer 100)
      initVal = (mn + mx) `div` 2
  tidx <- nextTextIdx
  return ([shamlet|
  <div .question>
    <div .question-text>
      {{text[#{tidx}]}}
    <div>
      <input type="range" min=#{mn} max=#{mx}
                  ng-model="results['#{name}']">
      <span>
        {{results['#{name}']}}
          |],
          [julius|
  console.log("Numeric question: #{name}");
  sc.text[#{Number (I tidx)}] = #{String qt};
  sc.results['#{name}'] = #{toJSON initVal};
  sc.postprocess['#{name}'] = postprocessor.numeric;
          |])

renderQuestion (ChoiceQuestion name qt _os cs) = do
  tidx <- nextTextIdx
  return ([shamlet|
  <div .question>
    <div .question-text>
      <span>
        {{text[#{tidx}]}}
      $forall Choice ct (LitExpr v) <- cs
        <label .radio>
          <span>
            #{ct}
          <input type="radio" ng-model="results['#{name}']"
                 value=#{toMarkup v} name=#{"radio_" <> name}>
          |],
          [julius|
  console.log("Choice question: #{name}");
  sc.text[#{Number (I tidx)}] = #{String qt};
  sc.results['#{name}'] = null;
          |])

renderQuestion (DropdownQuestion name qt _os cs) = do
  tidx <- nextTextIdx
  cidx <- nextUtilIdx
  return ([shamlet|
  <div .question>
    <div .question-text>
      <span>
        {{text[#{tidx}]}}
      <select ng-model="results['#{name}']"
              ng-options="c.label for c in utils[#{cidx}]">
          |],
          [julius|
  console.log("Dropdown question: #{name}");
  sc.text[#{Number (I tidx)}] = #{String qt};
  sc.utils[#{Number (I cidx)}] = #{cs};
  sc.results['#{name}'] = sc.utils[#{Number (I cidx)}][0];
  sc.postprocess['#{name}'] = postprocessor.choices;
          |])

renderQuestion (TextEntryQuestion name qt _os) = do
  tidx <- nextTextIdx
  return ([shamlet|
  <div .question>
    <div .question-text>
      <span>
        {{text[#{tidx}]}}
      <input type="text" ng-model="results['#{name}']">
          |],
          [julius|
  console.log("Text entry question: #{name}");
  sc.text[#{Number (I tidx)}] = #{String qt};
  sc.results['#{name}'] = '';
          |])

renderQuestion (TextDisplay qt _os) = do
  let qts = fscope True [] qt
  return ([shamlet|
  <div .question>
    <div .question-text>
      <span>
        {{#{preEscapedToMarkup $ toLazyText $ toJavascript qts}}}
          |],
          [julius|
  console.log("Text display...");
          |])
