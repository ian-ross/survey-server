module Main where

import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

import Language.ModuleDSL.Syntax
import Language.ModuleDSL.Pretty
import Language.ModuleDSL.Internal.Parser
import Language.ModuleDSL.Arbitrary ()


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ literalsGroup, idsGroup ]

literalsGroup :: TestTree
literalsGroup = testGroup "Literals"
 [ testProperty "Numeric literals" num_literals
 , testProperty "Boolean literals" bool_literals
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

idsGroup :: TestTree
idsGroup = testGroup "Identifiers"
 [ testProperty "General identifiers" ids
 ]

ids :: Name -> Bool
ids = roundTrip pName


-- Pretty-printer/parser round-trip testing property.
roundTrip :: (Eq a, Pretty a, Arbitrary a) => Parser a -> a -> Bool
roundTrip p ast = if null errs then res == ast else False
  where (res, errs) = parse ((,) <$ pSpaces <*> p <* pSpaces <*> pEnd) inp
        inp = createStr (LineColPos 0 0 0) (T.unpack $ prettyPrint ast)
