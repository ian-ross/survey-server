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
  arbitrary = oneof [ liftM2 NumericQuestion okstring arbitrary
                    , liftM3 ChoiceQuestion okstring arbitrary arbitrary ]

instance Arbitrary Choice where
  arbitrary = liftM2 Choice okstring arbitrary

instance Arbitrary Option where
  arbitrary = liftM2 Option arbitrary arbitrary

instance Arbitrary Literal where
  arbitrary = oneof [ liftM String okstring
                    , liftM Integer arbitrary
                    , liftM Double arbitrary
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
  arbitrary = elements
              [ NumNeg, NumAbs, NumFloor, NumCeil, LogNot, LogAny, LogAll ]

instance Arbitrary BinaryOp where
  arbitrary = elements
              [ NumAdd, NumSub, NumMul, NumDiv, NumPow, LogAnd, LogOr
              , CmpEq, CmpNEq, CmpGt, CmpGEq, CmpLt, CmpLeq
              , CmpEqCI, CmpNEqCI, CmpGtCI, CmpGEqCI, CmpLtCI, CmpLeqCI ]

instance Arbitrary Expr where
  arbitrary = sized expr
    where expr n
            | n == 0 = oneof [ liftM LitExpr arbitrary
                             , liftM RefExpr arbitrary ]
            | otherwise = oneof [ liftM LitExpr arbitrary
                                , liftM RefExpr arbitrary
                                , liftM2 UnaryExpr arbitrary subexpr
                                , liftM3 BinaryExpr arbitrary subexpr subexpr
                                , liftM2 FunExpr arbitrary (listOf subexpr) ]
            where subexpr = expr (n `div` 2)
