{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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


-- Arbitrary instances for all AST types.

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

okstring :: Gen Text
okstring = (T.pack . dupquotes) <$> sized (\n -> replicateM n okch)
  where okch = frequency [(10, elements [' ', '"']), (10, choose ('0', '9')),
                          (26, choose ('a', 'z')), (26, choose ('A', 'Z')),
                          (10, choose (' ', '\127') `suchThat` isPrint),
                          (5, choose (' ', '\1024') `suchThat` isPrint)]

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
                          , liftM RecordExpr (resize (n `div` 2) arbitrary) ]
            where subexpr = expr (n `div` 4)
