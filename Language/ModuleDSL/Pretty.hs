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
  pretty NumNeg = text "-"
  pretty NumAbs = text "abs"
  pretty NumFloor = text "floor"
  pretty NumCeil = text "ceil"
  pretty LogNot = text "not"
  pretty LogAny = text "any"
  pretty LogAll = text "all"

instance Pretty BinaryOp where
  pretty NumAdd =   text "+"
  pretty NumSub =   text "-"
  pretty NumMul =   text "*"
  pretty NumDiv =   text "/"
  pretty NumPow =   text "^"
  pretty LogAnd =   text "and"
  pretty LogOr =    text "or"
  pretty CmpEq =    text "=="
  pretty CmpNEq =   text "/="
  pretty CmpGt =    text ">"
  pretty CmpGEq =   text ">="
  pretty CmpLt =    text "<"
  pretty CmpLeq =   text "<="
  pretty CmpEqCI =  text "==@"
  pretty CmpNEqCI = text "/=@"
  pretty CmpGtCI =  text ">@"
  pretty CmpGEqCI = text ">=@"
  pretty CmpLtCI =  text "<@"
  pretty CmpLeqCI = text "<=@"

instance Pretty Expr where
  pretty (LitExpr lit) = pretty lit
  pretty (RefExpr name) = pretty name
  pretty (UnaryExpr NumNeg e) = pretty NumNeg <> pretty e
  pretty (UnaryExpr op e) = pretty op <+> pretty e
  pretty (BinaryExpr op e1 e2) =
    parens (pretty e1) <+> pretty op <+> parens (pretty e2)
  pretty (FunExpr f es) = pretty f <>
                          (parens $ hsep $ punctuate comma $ map pretty es)

instance Pretty Option where
  pretty (Option k v) = pretty k <> text " = " <> pretty v

instance Pretty [Option] where
  pretty os = brackets $ hsep $ punctuate comma $ map pretty os

instance Pretty Choice where
  pretty (Choice t v) = doubleQuotes (ttext t) <> text " => " <> pretty v

instance Pretty [Choice] where
  pretty cs = braces $ hsep $ punctuate comma $ map pretty cs

instance Pretty Question where
  pretty (NumericQuestion t os) =
    text "NumericQuestion" <+> doubleQuotes (ttext t) $$
    nest 2 (pretty os)
  pretty (ChoiceQuestion t os cs) =
    text "ChoiceQuestion" <+> doubleQuotes (ttext t) $$
    nest 2 (pretty os $$
            pretty cs)

instance Pretty TopLevel where
  pretty (Specialisation nm ps body) =
    text "Specialisation" <+> pretty nm <>
    parens (hsep $ punctuate comma $ map pretty ps) <+> text "=" $$
    nest 2 (pretty body)
  pretty (SurveyPage nm os qs) =
    text "SurveyPage" <+> pretty nm <+> pretty os $$
    (vcat $ map (\(n, q) -> pretty n <+> text "=" <+> pretty q) qs)

instance Pretty [TopLevel] where
  pretty ts = vcat $ punctuate (text "") $ map pretty ts

instance Pretty Module where
  pretty (Module nm os body) =
    text "Module" <+> pretty nm <+> pretty os $+$
    nest 2 (pretty body)
