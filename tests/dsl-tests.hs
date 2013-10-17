module Main where

import Data.Data
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.QuickCheck
--import Test.QuickCheck
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

import Language.ModuleDSL.Syntax
import Language.ModuleDSL.Pretty
import Language.ModuleDSL.Internal.Parser
import Language.ModuleDSL.Arbitrary ()


main :: IO ()
main = defaultMain tests
--main = verboseCheckWith (stdArgs { maxSize = 50 }) exprs

tests :: TestTree
tests = testGroup "Tests"
        [ testProperty "Numeric literals" num_literals
        , testProperty "Boolean literals" bool_literals
        , testProperty "String literals"  string_literals
        , testProperty "General identifiers" ids
        , localOption (QuickCheckMaxSize 50) $ testProperty "Expressions" exprs
        ]

num_literals :: Literal -> Property
num_literals x = isNumeric x ==> roundTrip pLiteral x
  where isNumeric (Integer _) = True
        isNumeric (Double _)  = True
        isNumeric _           = False

bool_literals :: Literal -> Property
bool_literals x = isBool x ==> roundTrip pLiteral x
  where isBool (Bool _) = True
        isBool _        = False

string_literals :: Literal -> Property
string_literals x = isString x ==> roundTrip pLiteral x
  where isString (String _) = True
        isString _          = False

ids :: Name -> Bool
ids = roundTrip pName

exprs :: Expr -> Bool
exprs = roundTripWith normalise pExpr

-- Pretty-printer/parser round-trip testing property.

roundTrip :: (Eq a, Pretty a, Arbitrary a, Data a) => Parser a -> a -> Bool
roundTrip = roundTripWith id

roundTripWith :: (Eq a, Pretty a, Arbitrary a, Data a) =>
              (a -> a) -> Parser a -> a -> Bool
roundTripWith norm p ast = if null errs then norm res == norm ast else False
  where (res, errs) = parse ((,) <$ pSpaces <*> p <* pSpaces <*> pEnd) inp
        inp = createStr (LineColPos 0 0 0) (T.unpack $ prettyPrint ast)
