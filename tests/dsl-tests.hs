module Main where

import qualified Data.Text as T
import Test.QuickCheck
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

import Language.ModuleDSL.Syntax
import Language.ModuleDSL.Pretty
import Language.ModuleDSL.Internal.Parser
import Language.ModuleDSL.Arbitrary ()


main :: IO ()
main = do
  quickCheck num_literals

num_literals :: Property
num_literals = verbose $ \x -> isNumeric x ==> roundTrip pLiteral x
  where isNumeric (Integer _) = True
        isNumeric (Double _)  = True
        isNumeric _           = False



-- Pretty-printer/parser round-trip testing property.
roundTrip :: (Eq a, Pretty a, Arbitrary a) => Parser a -> a -> Bool
roundTrip p ast = if null errs then res == ast else False
  where (res, errs) = parse ((,) <$ pSpaces <*> p <* pSpaces <*> pEnd) inp
        inp = createStr (LineColPos 0 0 0) (T.unpack $ prettyPrint ast)
