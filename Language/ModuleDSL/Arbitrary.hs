{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Arbitrary instances for module DSL AST types for round-trip
-- QuickCheck testing of parser.  Most of these are pretty
-- self-explanatory.
--
-- Note that these are all orphan instances, to avoid polluting
-- 'Language.ModuleDSL.Syntax' with them!
--
module Language.ModuleDSL.Arbitrary where

import Prelude
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Language.ModuleDSL.Syntax
import Language.ModuleDSL.Internal.Utils


instance Arbitrary Module where
  arbitrary = liftM3 Module arbitrary arbitrary arbitrary

instance Arbitrary TopLevel where
  arbitrary = oneof [ liftM3 Specialisation arbitrary arbitrary arbitrary
                    , liftM3 SurveyPage arbitrary arbitrary arbitrary ]

instance Arbitrary Question where
  arbitrary = oneof
              [ liftM3 NumericQuestion arbitrary okstring arbitrary
              , liftM4 ChoiceQuestion arbitrary okstring arbitrary arbitrary
              , liftM4 DropdownQuestion arbitrary okstring arbitrary arbitrary
              , liftM3 TextEntryQuestion arbitrary okstring arbitrary
              , liftM2 TextDisplay arbitrary arbitrary ]

instance Arbitrary Choice where
  arbitrary = liftM2 Choice okstring arbitrary

instance Arbitrary Options where
  arbitrary = liftM (Options . RecordExpr) arbitrary

instance Arbitrary Literal where
  arbitrary = oneof [ liftM String okstring
                    , liftM (Integer . abs) arbitrary
                    , liftM (Double . abs) arbitrary
                    , liftM Bool arbitrary
                    , return Null ]

-- | Generator for string contents that includes Unicode characters
-- and handles quotation of double quotes as required by the module
-- DSL parser.
okstring :: Gen Text
okstring = (T.pack . dupquotes) <$> sized (\n -> replicateM n okch)
  where okch = frequency [(10, elements [' ', '"']), (10, choose ('0', '9')),
                          (26, choose ('a', 'z')), (26, choose ('A', 'Z')),
                          (10, choose (' ', '\127') `suchThat` isPrint),
                          (5, choose (' ', '\1024') `suchThat` isPrint)]

-- | Generator for arbitrary IDs: makes period-separated sequences of
-- names that aren't too long.
instance Arbitrary Name where
  arbitrary = Name <$> T.intercalate "." . map T.pack . take 3 <$> listOf1 comp
    where comp = (:) <$> inich <*> listOf ch
          inich = frequency [(26, choose ('a', 'z')), (26, choose ('A', 'Z'))]
          ch = frequency [(26, choose ('a', 'z')), (26, choose ('A', 'Z')),
                          (2, elements ['_', '-']), (10, choose ('0', '9'))]

instance Arbitrary UnaryOp where
  arbitrary = elements [ NegOp, NotOp ]

instance Arbitrary BinaryOp where
  arbitrary = elements [ AddOp, SubOp, MulOp, DivOp, PowOp, AndOp, OrOp
                       , EqOp, NEqOp, GtOp, GEqOp, LtOp, LeqOp
                       , EqCIOp, NEqCIOp, GtCIOp, GEqCIOp, LtCIOp, LeqCIOp ]

-- | Use standard approach for controlling size of arbitrary values of
-- recursive data structures.  (The divisors of 2 and 4 below are
-- determined empirically as yielding reasonable sizes of values for
-- testing.)
instance Arbitrary Expr where
  arbitrary = sized expr
    where expr n
            | n == 0 = oneof [ liftM LitExpr arbitrary
                             , liftM RefExpr arbitrary ]
            | otherwise = oneof
                          [ liftM LitExpr arbitrary
                          , liftM RefExpr arbitrary
                          , liftM2 UnaryExpr arbitrary subexpr
                          , liftM3 BinaryExpr arbitrary subexpr subexpr
                          , liftM2 FunExpr arbitrary (listOf subexpr)
                          , liftM3 IfThenElseExpr subexpr subexpr subexpr
                          , liftM RecordExpr (resize (n `div` 2) arbitrary)
                          , liftM ArrayExpr (resize (n `div` 2) arbitrary) ]
            where subexpr = expr (n `div` 4)
