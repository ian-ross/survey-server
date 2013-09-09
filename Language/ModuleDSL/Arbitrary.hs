{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.ModuleDSL.Arbitrary where

import Prelude
import Test.QuickCheck
import Control.Monad

import Language.ModuleDSL.Syntax
import Language.ModuleDSL.Pretty
import Language.ModuleDSL.Parser

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

instance Arbitrary Value where
  arbitrary = oneof [ liftM String okstring
                    , liftM Integer arbitrary
                    , liftM Double arbitrary
                    , liftM Bool arbitrary
                    , return Null ]

instance Arbitrary Name where
  arbitrary = liftM Name nameGenerator
    where nameGenerator = liftM2 (:) inich $ sized (\n -> replicateM n ch)
          inich = frequency [(26, choose ('a', 'z'))]
          ch = frequency [(26, choose ('a', 'z')), (26, choose ('A', 'Z')),
                          (2, elements ['_', '-']), (10, choose ('0', '9'))]

-- THIS IS A DEFICIENCY IN THE STRING PARSER: WE SHOULD SUPPORT SOME
-- SORT OF ESCAPING FOR QUOTES AND OTHER SPECIAL CHARACTERS.
okstring :: Gen String
okstring = sized (\n -> replicateM n $ elements $
                        ['\32', '\33'] ++ ['\35'..'\127'])

-- | Pretty-printer/parser round-trip testing property.
checkModuleParser :: Module -> Bool
checkModuleParser ast =
  case parseModule (prettyPrint ast) of
    Left _ -> False
    Right a -> ast == a

runChecks :: IO ()
runChecks = quickCheck checkModuleParser
