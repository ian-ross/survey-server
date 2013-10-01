{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Language.ModuleDSL.Parser
       ( parseModule
       , formatErrors ) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

import Language.ModuleDSL.Syntax
import Language.ModuleDSL.Internal.Parser


-- | Format errors for display.
formatErrors :: [Error LineColPos] -> [String]
formatErrors = map formatError
  where formatError (Inserted s p e) =
          formatPos p ++ formatExpects e ++ "(inserted " ++ s ++ ")"
        formatError (Deleted s p e) =
          formatPos p ++ formatExpects e ++ "(deleted " ++ s ++ ")"
        formatError (Replaced s1 s2 p e) =
          formatPos p ++ formatExpects e ++
          "(replaced " ++ s1 ++ " with " ++ s2 ++ ")"
        formatError (DeletedAtEnd s) = "Extra input at end of file: " ++ s
        formatPos (LineColPos l c _) =
          show (l+1) ++ ":" ++ show (c+1) ++ "  Parse error: "
        formatExpects [] = "expecting nothing "
        formatExpects [a] = "expecting " ++ a ++ " "
        formatExpects (a:as) =
          "expecting one of [" ++ a ++ concat (map (", " ++) as) ++ "] "

-- | Parse a single module from a string, returning either the module
-- definition or a list of errors.
parseModule :: Text -> Either [Error LineColPos] Module
parseModule inp = if null errs then Right res else Left errs
  where (res, errs) = parse ((,) <$ pSpaces <*> pModule <*> pEnd)
                            (createStr (LineColPos 0 0 0) (T.unpack inp))
