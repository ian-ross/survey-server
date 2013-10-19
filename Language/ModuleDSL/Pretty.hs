{-# LANGUAGE FlexibleInstances #-}
module Language.ModuleDSL.Pretty
       ( Pretty(..)
       , prettyPrint ) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint

import Language.ModuleDSL.Syntax
import Language.ModuleDSL.Internal.Utils


class Pretty a where
  pretty :: a -> Doc

prettyPrint :: Pretty a => a -> Text
prettyPrint = T.pack . render . pretty

ttext :: Text -> Doc
ttext = text . T.unpack

instance Pretty Literal where
  pretty (String s) = doubleQuotes . ttext . T.pack . dupquotes . T.unpack $ s
  pretty (Integer i) = integer i
  pretty (Double d) = double d
  pretty (Bool True) = text "true"
  pretty (Bool False) = text "false"
  pretty Null = text "null"

instance Pretty Name where
  pretty (Name n) = ttext n

instance Pretty UnaryOp where
  pretty NegOp = text "-"
  pretty NotOp = text "!"

instance Pretty BinaryOp where
  pretty AddOp =   text "+"
  pretty SubOp =   text "-"
  pretty MulOp =   text "*"
  pretty DivOp =   text "/"
  pretty PowOp =   text "^"
  pretty CatOp =   text "<>"
  pretty AndOp =   text "and"
  pretty OrOp =    text "or"
  pretty EqOp =    text "=="
  pretty NEqOp =   text "!="
  pretty GtOp =    text ">"
  pretty GEqOp =   text ">="
  pretty LtOp =    text "<"
  pretty LeqOp =   text "<="
  pretty EqCIOp =  text "@=="
  pretty NEqCIOp = text "@!="
  pretty GtCIOp =  text "@>"
  pretty GEqCIOp = text "@>="
  pretty LtCIOp =  text "@<"
  pretty LeqCIOp = text "@<="

instance Pretty Expr where
  pretty (LitExpr lit) = pretty lit
  pretty (RefExpr name) = pretty name
  pretty (UnaryExpr op e) = pretty op <+> parens (pretty e)
  pretty (BinaryExpr op e1 e2) =
    parens (pretty e1) <+> pretty op <+> parens (pretty e2)
  pretty (FunExpr f es) = pretty f <>
                          (parens $ hsep $ punctuate comma $ map pretty es)
  pretty (IfThenElseExpr i t e) = text "if" <+> pretty i $$
                                  nest 2 (text "then" <+> pretty t $$
                                          text "else" <+> pretty e)
  pretty (RecordExpr fs) =
    brackets $ hsep $ punctuate comma $
    map (\(k, v) -> pretty k <+> text "=" <+> pretty v) fs

instance Pretty Options where
  pretty (Options (RecordExpr [])) = empty
  pretty (Options r) = pretty r

instance Pretty Choice where
  pretty (Choice t v) = doubleQuotes (ttext t) <> text " => " <> pretty v

instance Pretty [Choice] where
  pretty cs = braces $ hsep $ punctuate comma $ map pretty cs

instance Pretty Question where
  pretty (NumericQuestion n t os) =
    pretty n <+> text "=" <+>
      text "NumericQuestion" <+> doubleQuotes (ttext t) $$
        nest 2 (pretty os)
  pretty (ChoiceQuestion n t os cs) =
    pretty n <+> text "=" <+>
      text "ChoiceQuestion" <+> doubleQuotes (ttext t) $$
        nest 2 (pretty os $$ pretty cs)
  pretty (DropdownQuestion n t os cs) =
    pretty n <+> text "=" <+>
      text "DropdownQuestion" <+> doubleQuotes (ttext t) $$
        nest 2 (pretty os $$ pretty cs)
  pretty (TextEntryQuestion n t os) =
    pretty n <+> text "=" <+>
      text "TextEntryQuestion" <+> doubleQuotes (ttext t) $$
        nest 2 (pretty os)
  pretty (TextDisplay t os) =
    text "TextDisplay" <+> pretty t $$
    nest 2 (pretty os)

instance Pretty TopLevel where
  pretty (Specialisation nm ps body) =
    text "Specialisation" <+> pretty nm <>
    parens (hsep $ punctuate comma $ map pretty ps) <+> text "=" $$
    nest 2 (pretty body)
  pretty (SurveyPage nm os qs) =
    text "SurveyPage" <+> pretty nm <+> pretty os $$ (vcat $ map pretty qs)
  pretty (Function nm ps body) =
    text "Function" <+> pretty nm <+>
    parens (hsep $ punctuate comma $ map pretty ps) <+> text "=" $$
    nest 2 (pretty body)

instance Pretty [TopLevel] where
  pretty ts = vcat $ punctuate (text "") $ map pretty ts

instance Pretty Module where
  pretty (Module nm os body) =
    text "Module" <+> pretty nm <+> pretty os $+$
    nest 2 (pretty body)
